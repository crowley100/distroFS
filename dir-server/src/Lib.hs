{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Crypto.BCrypt
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,newManager)

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           UseHaskellAPI
import           UseHaskellAPIServer

-- ip defaulting to local host (may change to dynamically assign ips)
initFileServers :: IO ()
initFileServers = do
  warnLog $ "populating db records with default values"
  withMongoDbConnection $ do
    let ip = "127.0.0.1"
        name1 = "fs1"
        name2 = "fs2"
        name3 = "fs3"
        port1 = "8081"
        port2 = "8082"
        port3 = "8083"
    let value1 = (FsInfo name1 ip port1)
        value2 = (FsInfo name2 ip port2)
        value3 = (FsInfo name3 ip port3)
        files1 = (FsContents name1 [])
        files2 = (FsContents name2 [])
        files3 = (FsContents name3 [])
    -- enter file server meta data
    repsert (select  ["directory" =: name1] "FS_INFO") $ toBSON value1
    repsert (select  ["directory" =: name2] "FS_INFO") $ toBSON value2
    repsert (select  ["directory" =: name3] "FS_INFO") $ toBSON value3
    -- specify file server contents
    repsert (select  ["dirName" =: name1] "CONTENTS_RECORD") $ toBSON files1
    repsert (select  ["dirName" =: name2] "CONTENTS_RECORD") $ toBSON files2
    repsert (select  ["dirName" =: name3] "CONTENTS_RECORD") $ toBSON files3

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog $ "Starting directory-service."
  let settings = setPort 8000 $ setLogger aplogger defaultSettings -- port change?
  initFileServers -- possible changes here!
  runSettings settings app

app :: Application
app = serve api dirService

api :: Proxy DirAPI
api = Proxy

dirService :: Server DirAPI
dirService = lsDir
        :<|> lsFile
        :<|> fileQuery
        :<|> mapFile
  where
    lsDir :: Handler [FsContents]
    lsDir = liftIO $ do
      warnLog $ "Client listing directories"
      withMongoDbConnection $ do
        dirs <- find (select [] "CONTENTS_RECORD") >>= drainCursor
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsContents) dirs

    lsFile :: Maybe String -> Handler [FsContents]
    lsFile (Just directory) = liftIO $ do
      warnLog $ "Client listing files in directory: " ++ directory
      withMongoDbConnection $ do
        contents <- find (select ["dirName" =: directory] "CONTENTS_RECORD") >>= drainCursor
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsContents) contents

    fileQuery :: Message -> Handler [FileRef]
    fileQuery query@(Message fName fDir) = liftIO $ do
      warnLog $ "Client querying file ["++fName++"] in directory: " ++ fDir
      withMongoDbConnection $ do
        let filepath = (fName ++ fDir)
        docs <- find (select ["filePath" =: filepath] "FILEREF_RECORD") >>= drainCursor
        let result = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) docs
        case result of
          (ref:_) -> return [ref]
          [] -> return ([] :: [FileRef]) -- Can handle empty list on client side

    mapFile :: Message -> Handler [FileRef]
    mapFile val@(Message fName fDir) = liftIO $ do
      warnLog $ "Client mapping file ["++fName++"] in directory: " ++ fDir
      currentTime <- getCurrentTime
      let myTime = (show currentTime)
      withMongoDbConnection $ do
        let filepath = (fName ++ fDir)
        docs <- find (select ["filePath" =: filepath] "FILEREF_RECORD") >>= drainCursor
        let result = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) docs
        case result of
          ((FileRef somePath someID t someIP somePort):_) -> do
            return [(FileRef somePath someID myTime someIP somePort)]
          [] -> do
            fsInfo <- find (select ["myName" =: fDir] "FS_INFO") >>= drainCursor
            let fs = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsInfo) fsInfo
            case fs of
              [] -> return ([] :: [FileRef]) -- Can handle empty list on client side
              (info@(FsInfo _ ip port):_) -> do
                -- update contents record
                contents <- find (select ["dirName" =: fDir] "CONTENTS_RECORD") >>= drainCursor
                let fList = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsContents) contents
                case fList of
                  ((FsContents _ files):_) -> liftIO $ do
                    let newContents = (FsContents fDir (files ++ [fName]))
                    withMongoDbConnection $ upsert (select  ["dirName" =: fDir] "CONTENTS_RECORD") $ toBSON newContents
                  [] -> liftIO $ do warnLog $ "ERROR: inconsistent db record (CONTENTS_RECORD)"
                ids <- find (select ["directory" =: fDir] "ID_RECORD") >>= drainCursor
                let newID = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileID) ids
                case newID of
                  ((FileID dirName myID):_) -> liftIO $ do
                    let retID = myID
                        newID = (show ((read myID) + 1))
                    let value = FileID dirName newID
                    withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                    return [(FileRef filepath retID myTime ip port)]
                  [] -> liftIO $ do
                    let retID = "0"
                        newID = "1"
                    let value = FileID fDir newID
                    withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                    return [(FileRef filepath retID myTime ip port)]

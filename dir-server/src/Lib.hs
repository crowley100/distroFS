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

type MyDirAPI = "lsDir"                   :> Get '[JSON] [FsContents]
           :<|> "lsFile"                  :> QueryParam "name" String :> Get '[JSON] [FsContents]
           :<|> "fileQuery"               :> ReqBody '[JSON] Message :> Get '[JSON] [FileRef]
           :<|> "mapFile"                 :> ReqBody '[JSON] Message :> Get '[JSON] [FileRef]
           :<|> "ping"                    :> ReqBody '[JSON] Message :> Post '[JSON] Bool
           :<|> "registerFS"              :> ReqBody '[JSON] Message3 :> Post '[JSON] Bool

-- ip defaulting to local host (currently only 2 FS running for testing purposess)
{-
initFileServers :: IO ()
initFileServers = do
  warnLog $ "populating db records with default values"
  withMongoDbConnection $ do
    let server11 = FsAttributes
        server12 = FsAttributes
        server13 = FsAttributes
        server21 = FsAttributes
        server22 = FsAttributes
        server23 = FsAttributes
    let servers1 = FsInfo ("fs1") (server1:([server12] ++ [server]))
        servers2 = FsInfo
    let value1 = (FsInfo name1 ip1 port1)
        value2 = (FsInfo name2 ip2 port2)
        files1 = (FsContents name1 [])
        files2 = (FsContents name2 [])
    -- enter file server meta data
    upsert (select  ["directory" =: name1] "FS_INFO") $ toBSON value1
    upsert (select  ["directory" =: name2] "FS_INFO") $ toBSON value2
    -- specify file server contents
    upsert (select  ["dirName" =: name1] "CONTENTS_RECORD") $ toBSON files1
    upsert (select  ["dirName" =: name2] "CONTENTS_RECORD") $ toBSON files2 -}

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog $ "Starting directory-service."
  let settings = setPort 8000 $ setLogger aplogger defaultSettings -- port change?
  --initFileServers -- possible changes here!
  runSettings settings app

app :: Application
app = serve api dirService

api :: Proxy MyDirAPI
api = Proxy

dirService :: Server MyDirAPI
dirService = lsDir
        :<|> lsFile
        :<|> fileQuery
        :<|> mapFile
        :<|> ping
        :<|> registerFS
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

    fileQuery :: Message -> Handler [SendFileRef]
    fileQuery query@(Message fName fDir) = liftIO $ do
      warnLog $ "Client querying file ["++fName++"] in directory: " ++ fDir
      withMongoDbConnection $ do
        let filepath = (fDir ++ fName)
        findRef <- find (select ["filePath" =: filepath] "FILEREF_RECORD") >>= drainCursor
        let result = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) findRef
        case result of
          (ref@(FileRef fp fid fts):_) -> do
            -- lookup fDir to get list of replicas
            findFS <- find (select ["myName" =: fDir] "FS_INFO") >>= drainCursor
            let servers = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsInfo) findRef
            case servers of
              [] -> return ([] :: [SendFileRef]) -- directory not found
              ((FsInfo name serverList):_) -> do
                -- process to pick a replica (load balancing - parse ip/port)
                -- WORKING HERE
                return [(SendFileRef fp fid fts newIp newPort)]
          [] -> return ([] :: [SendFileRef]) -- file not found

    mapFile :: Message -> Handler [SendFileRef]
    mapFile val@(Message fName fDir) = liftIO $ do
      warnLog $ "Client mapping file ["++fName++"] in directory: " ++ fDir
      currentTime <- getCurrentTime
      let myTime = (show currentTime)
      withMongoDbConnection $ do
        let filepath = (fDir ++ fName)
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
                        updatedID = (show ((read myID) + 1))
                    let value = FileID dirName updatedID
                        result = (FileRef filepath retID myTime ip port)
                    withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                    withMongoDbConnection $ upsert (select  ["filePath" =: filepath] "FILEREF_RECORD") $ toBSON result
                    return [(FileRef filepath retID myTime ip port)]
                  [] -> liftIO $ do
                    let retID = "0"
                        updatedID = "1"
                    let value = FileID fDir updatedID
                        result = (FileRef filepath retID myTime ip port)
                    withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                    withMongoDbConnection $ upsert (select  ["filePath" =: filepath] "FILEREF_RECORD") $ toBSON result
                    return [(FileRef filepath retID myTime ip port)]

    -- affirm fs is still alive
    ping :: Message -> Handler Bool
    ping msg@(Message "thing1" "thing2") = do
      return True

    -- register a file server
    registerFS :: Message3 -> Handler Bool
    registerFS msg3@(Message3 dirName fsIP fsPort) = liftIO $ do
      warnLog $ "Registering file server associated with [" ++ dirName ++ "]"

      return True

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

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting file-service."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings -- port change?
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
    -- client wants file(x)
    -- server checks db mapping of (x,FILE_ID)
    -- where FILE_ID is of type (ID, FS_IP/PORT)
    -- server responds with FILE_ID

    -- temp implementation
    lsDir :: Handler [ResponseData]
    lsDir = liftIO $ do
      withMongoDbConnection $ do
        return [(ResponseData $ "temp")]

    -- temp implementation
    lsFile :: Maybe String -> Handler [ResponseData]
    lsFile (Just temp) = liftIO $ do
      withMongoDbConnection $ do
        return [(ResponseData $ "temp")]

    fileQuery :: Message -> Handler [FileRef]
    fileQuery query@(Message fName fDir) = liftIO $ do
      warnLog $ "Client querying file ["++fName++"] in directory: " ++ fDir
      withMongoDbConnection $ do
        docs <- find (select ["name" =: (fName ++ fDir)] "FILEREF_RECORD") >>= drainCursor
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) docs

    mapFile :: Message -> Handler [FileRef]
    mapFile val@(Message fName fDir) = liftIO $ do
      warnLog $ "Client mapping file ["++fName++"] in directory: " ++ fDir
      withMongoDbConnection $ do
        let filepath = (fName ++ fDir)
        docs <- find (select ["filePath" =: filepath] "FILEREF_RECORD") >>= drainCursor
        let result = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) docs
        case result of
          (ref@(FileRef _ fID ip port):_) -> return [ref]
          [] -> do
            -- determine unique directory id (pull from db and update)
            -- upsert new FileRef entery (using record: (fdir,(ip,port)))
            -- new FileRef: (id,ip,port)
            fsInfo <- find (select ["myName" =: fDir] "FS_INFO") >>= drainCursor
            let fs = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsInfo) fsInfo
            case fs of
              [] -> return ([] :: [FileRef])
              (info@(FsInfo _ ip port):_) -> do
                ids <- find (select ["directory" =: fDir] "ID_RECORD") >>= drainCursor
                -- Int probably wont work, change to string and match API
                let newID = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileID) ids
                case newID of
                  (FileID dirName myID:_) -> liftIO $ do
                    let retID = myID
                        newID = (show ((read myID) + 1))
                    let value = FileID dirName newID
                    withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                    return [(FileRef filepath retID ip port)]
                  [] -> liftIO $ do
                    let retID = "0"
                        newID = "1"
                    let value = FileID fDir newID
                    withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                    return [(FileRef filepath retID ip port)]

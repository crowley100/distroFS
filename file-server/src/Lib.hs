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
  let settings = setPort 8081 $ setLogger aplogger defaultSettings -- port change!!!
  runSettings settings app

app :: Application
app = serve api fileService

api :: Proxy FileAPI
api = Proxy

fileService :: Server FileAPI
fileService = download
         :<|> upload

  where -- using Message type to send (fPath, fConents)
    download :: Maybe String -> Handler [Message] -- ResponseData?
    download (Just fPath) = liftIO $ do
      warnLog $ "Attempting to download file: [" ++ fPath ++ "] from db."
      withMongoDbConnection $ do
        docs <- find (select ["name" =: fPath] "FILE_RECORD") >>= drainCursor
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

    download Nothing = liftIO $ do
      warnLog $ "No file specified to download."
      return $ ([] :: [Message])

    -- lock here prior to upload of file
    upload :: Message -> Handler Bool
    upload myFile@(Message fPath _) = liftIO $ do
      warnLog $ "Uploading file to db: [" ++ fPath ++ "]"
      withMongoDbConnection $ upsert (select ["name" =: fPath] "FILE_RECORD") $ toBSON myFile
      return True

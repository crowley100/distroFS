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

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog $ "Starting transaction-service."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings -- port change?
  runSettings settings app

app :: Application
app = serve api transService

api :: Proxy TransAPI
api = Proxy

transService :: Server TransAPI -- currently only phase 1 ???
transService = beginTransaction
          :<|> tUpload
          :<|> commit
          :<|> abort
  where
    beginTransaction :: Handler ResponseData
    beginTransaction = liftIO $ do
      warnLog $ "Client starting a new transaction"
      -- check if client already has transaction on client side
      return $ ResponseData "temp"

    tUpload :: FileTransaction -> Handler Bool
    tUpload (FileTransaction transID fileRef fContents) = liftIO $ do
      warnLog $ "Client uploading a modification to the transaction"
      return True

    commit :: String -> Handler Bool
    commit transID = liftIO $ do
      warnLog $ "Client committing modifications in the transaction"
      return True

    abort :: String -> Handler Bool
    abort transID = liftIO $ do
      warnLog $ "Client aborting modifications in the transaction"
      return True

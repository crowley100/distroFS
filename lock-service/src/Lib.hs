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
--import           RestClient
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
  warnLog "Starting lock-service."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings -- port change?
  runSettings settings app

app :: Application
app = serve api lockService

api :: Proxy LockAPI
api = Proxy

lockService :: Server LockAPI
lockService = lock
         :<|> unlock
         :<|> locked

  where
    lock :: String -> Handler Bool
    lock fname = liftIO $ do
      warnLog $ "attempting to lock file: [" ++ fname ++ "]"
      withMongoDbConnection $ do
        docs <- find (select ["fName" =: fname] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          [(Lock _ True)] -> return False -- islocked
          [(Lock _ False)] -> liftIO $ do
              withMongoDbConnection $ upsert (select ["fName" =: fname] "LOCK_RECORD") $ toBSON $ (Lock fname True)
              return True -- file sucessfully locked
          [] -> return False -- file does not exist
    lock _ = do
      return False

    unlock :: String -> Handler Bool
    unlock fname = liftIO $ do
      warnLog $ "attempting to unlock file: [" ++ fname ++ "]"
      withMongoDbConnection $ do
        docs <- find (select ["fName" =: fname] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          [(Lock _ True)] -> liftIO $ do
            withMongoDbConnection $ upsert (select ["fName" =: fname] "LOCK_RECORD") $ toBSON $ (Lock fname False)
            return True
          [(Lock _ False)] -> return False -- file is locked
          [] -> return False -- file does not exist
    unlock _ = do
      return False

    locked :: Maybe String -> Handler Bool -- same as searchMessage
    locked (Just fname) = liftIO $ do
      warnLog $ "checking if file: [" ++ fname ++ "] is locked"
      withMongoDbConnection $ do
        docs <- find (select ["fName" =: fname] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          [(Lock _ True)] -> return True
          otherwise -> return False
    locked Nothing = do
      return False

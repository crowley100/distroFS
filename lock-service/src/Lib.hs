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
    -- attempts to lock a file for a specified user
    lock :: Message3 -> Handler Bool
    lock (Message3 fname uName ticket) = liftIO $ do
      warnLog $ "attempting to lock file: [" ++ fname ++ "]"
      withMongoDbConnection $ do
        docs <- find (select ["fName" =: fname] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          ((Lock _ True _):_) -> return False -- islocked
          ((Lock _ False _):_) -> liftIO $ do
              withMongoDbConnection $ upsert (select ["fName" =: fname] "LOCK_RECORD") $ toBSON $ (Lock fname True uName)
              return True -- file sucessfully locked
          [] -> liftIO $ do -- file does not exist
              withMongoDbConnection $ upsert (select ["fName" =: fname] "LOCK_RECORD") $ toBSON $ (Lock fname True uName)
              return True
    lock _ = do
      return False

    -- attempts to unlock a file for a specified user
    unlock :: Message3 -> Handler Bool
    unlock (Message3 fname uName ticket) = liftIO $ do
      warnLog $ "attempting to unlock file: [" ++ fname ++ "]"
      withMongoDbConnection $ do
        docs <- find (select ["fName" =: fname] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          ((Lock _ True owner):_) -> liftIO $ do
            case (uName == owner) of
              True -> do
                withMongoDbConnection $ upsert (select ["fName" =: fname] "LOCK_RECORD") $ toBSON $ (Lock fname False uName)
                return True
              False -> do
                putStrLn $ "Hey! " ++ uName ++ "doesn't own " ++ fname
                return False
          ((Lock _ False _):_) -> return True -- file is already unlocked
          [] -> return False -- file does not exist
    unlock _ = do
      return False

    -- checks if lock is available
    locked :: Message -> Handler Bool -- same as searchMessage
    locked (Message fname ticket) = liftIO $ do
      warnLog $ "checking if file: [" ++ fname ++ "] is locked"
      withMongoDbConnection $ do
        docs <- find (select ["fName" =: fname] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          ((Lock _ True _):_) -> return True
          otherwise -> return False

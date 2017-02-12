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
import           UseHaskellAPITypes



startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting lock-service."
  let settings = setPort 8002 $ setLogger aplogger defaultSettings -- port change?
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
    -- | Attempts to lock a file for a specified user.
    lock :: Message3 -> Handler Bool
    lock (Message3 encPath encName ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let fPath = myDecryptAES (aesPad seshKey) encPath
          uName = myDecryptAES (aesPad seshKey) encName
      warnLog $ "attempting to lock file: [" ++ fPath ++ "]"
      withMongoDbConnection $ do
        docs <- find (select ["fPath" =: fPath] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          ((Lock _ True _):_) -> return False -- islocked
          ((Lock _ False _):_) -> liftIO $ do
              withMongoDbConnection $ upsert (select ["fPath" =: fPath] "LOCK_RECORD") $ toBSON $ (Lock fPath True uName)
              return True -- file sucessfully locked
          [] -> liftIO $ do -- file does not exist
              withMongoDbConnection $ upsert (select ["fPath" =: fPath] "LOCK_RECORD") $ toBSON $ (Lock fPath True uName)
              return True
    lock _ = do
      return False

    -- | Attempts to unlock a file for a specified user.
    unlock :: Message3 -> Handler Bool
    unlock (Message3 encPath encName ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let fPath = myDecryptAES (aesPad seshKey) encPath
          uName = myDecryptAES (aesPad seshKey) encName
      warnLog $ "attempting to unlock file: [" ++ fPath ++ "]"
      withMongoDbConnection $ do
        docs <- find (select ["fPath" =: fPath] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          ((Lock _ True owner):_) -> liftIO $ do
            case (uName == owner) of
              True -> do
                withMongoDbConnection $ upsert (select ["fPath" =: fPath] "LOCK_RECORD") $ toBSON $ (Lock fPath False uName)
                return True
              False -> do
                putStrLn $ "Hey! " ++ uName ++ "doesn't own " ++ fPath
                return False
          ((Lock _ False _):_) -> return True -- file is already unlocked
          [] -> return False -- file does not exist
    unlock _ = do
      return False

    -- | Checks if lock is available.
    locked :: Message -> Handler Bool -- same as searchMessage
    locked (Message encPath ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let fPath = myDecryptAES (aesPad seshKey) encPath
      warnLog $ "checking if file: [" ++ fPath ++ "] is locked"
      withMongoDbConnection $ do
        docs <- find (select ["fPath" =: fPath] "LOCK_RECORD") >>= drainCursor
        let x = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case x of
          ((Lock _ True _):_) -> return True
          otherwise -> return False

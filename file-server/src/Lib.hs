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
import           UseHaskellAPIClient
import           UseHaskellAPITypes
import           Control.Concurrent (forkIO, threadDelay)

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting file-service."
  envPort <- fsPort
  let myPort = (read envPort) :: Int
  let settings = setPort myPort $ setLogger aplogger defaultSettings -- port change!!!
  newApp <- app envPort
  runSettings settings newApp

app :: String -> IO Application
app port = do
  iExist port
  forkIO $ stillAlive 10 port
  return $ serve api fileService

api :: Proxy FileAPI
api = Proxy

fileService :: Server FileAPI
fileService = download
         :<|> upload
         :<|> updateShadowDB
         :<|> pushTransaction
         :<|> replicateFile
  where
    -- | Uses mapping from directory service, forwarded through the client,
    --  to return requested file contents.
    download :: Message -> Handler [Message]
    download (Message encFPath ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let fPath = myDecryptAES (aesPad seshKey) (encFPath)
      warnLog $ "Attempting to download file: [" ++ fPath ++ "] from db."
      withMongoDbConnection $ do
        docs <- find (select ["name" =: fPath] "FILE_RECORD") >>= drainCursor
        let file = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs
        case file of
          ((Message name text):_) -> do
            let encName = myEncryptAES (aesPad seshKey) (name)
                encText = myEncryptAES (aesPad seshKey) (text)
            return [(Message encName encText)]
          otherwise -> return ([] :: [Message])

    -- | Stores file contents at mapping from directory service, forwarded through the client.
    --  Then communicates with directory service in order to propagate the change to replica servers.
    upload :: Message3 -> Handler Bool
    upload (Message3 encFPath encText ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let fPath = myDecryptAES (aesPad seshKey) (encFPath)
          contents = myDecryptAES (aesPad seshKey) (encText)
      let myFile = (Message fPath contents)
      warnLog $ "Uploading file to db: [" ++ fPath ++ "] and propagating replicas"
      withMongoDbConnection $ upsert (select ["name" =: fPath] "FILE_RECORD") $ toBSON myFile
      -- propagate change
      myDir <- fsName -- get env vairable
      warnLog $ "DIRNAME: " ++ myDir
      propagate <- servDoCall (getPropagationInfo (Message myDir "ticket")) dirPort
      case propagate of
        Left e -> warnLog $ "propagation failure: " ++ (show e)
        Right replicas -> broadcast myFile replicas
      return True

    -- | Enters a file change into an intermediary shadow record associated with
    --  a particular transaction.
    updateShadowDB :: Shadow -> Handler Bool
    updateShadowDB (Shadow encTID file@(Message encFID encContents)) = liftIO $ do
      let tID = myDecryptAES (aesPad sharedSeed) (encTID)
          fId = myDecryptAES (aesPad sharedSeed) (encFID)
          fContents = myDecryptAES (aesPad sharedSeed) (encContents)
      let file = (Message fId fContents)
      warnLog $ "Entering [" ++ fId ++ "] to ready to commit state."
      myName <- fsName
      let retVal = (Message tID (myName ++ fId))
      withMongoDbConnection $ do
        findShadow <- find (select ["trID" =: tID] "SHADOW_RECORD") >>= drainCursor
        let myShadow = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowInfo) findShadow
        case myShadow of
          ((ShadowInfo _ files):_) -> liftIO $ do
            let new = (ShadowInfo tID (files ++ [file]))
            withMongoDbConnection $ upsert (select ["trID" =: tID] "SHADOW_RECORD") $ toBSON new
          [] -> liftIO $ do
            let new = (ShadowInfo tID [file])
            withMongoDbConnection $ upsert (select ["trID" =: tID] "SHADOW_RECORD") $ toBSON new
      -- send ready to commit
      warnLog $ "Responding to transaction server!"
      resp <- servDoCall (readyCommit retVal) transPort
      case resp of
        Left err -> warnLog $ "ERROR: " ++ (show err)
        Right True -> warnLog $ "I got a true response."
        Right False -> warnLog $ "I got a false response"
      return True

    -- | Move changes associated with a given transaction from the shadow record
    --  to the real db. Propagate these changes to replica file servers.
    pushTransaction :: String -> Handler Bool
    pushTransaction encTID = liftIO $ do
      let tID = myDecryptAES (aesPad sharedSeed) (encTID)
      warnLog $ "Moving shadow entries for [" ++ tID ++ "] to storage."
      myDir <- fsName -- get env vairable
      propagate <- servDoCall (getPropagationInfo (Message myDir "ticket")) dirPort
      case propagate of -- propagate transaction to replicas
        Left e -> warnLog $ "propagation failure: " ++ (show e)
        Right replicas -> do
          findEntries <- withMongoDbConnection $ find (select ["trID" =: tID] "SHADOW_RECORD") >>= drainCursor
          let entries = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowInfo) findEntries
          case entries of
            ((ShadowInfo _ files):_) -> do
              myCommit files replicas
              withMongoDbConnection $ delete (select ["trID" =: tID] "SHADOW_RECORD")
            otherwise -> putStrLn "nothing left to commit..."
      return True

    -- | Receive update from primary file server.
    replicateFile :: Message -> Handler Bool
    replicateFile (Message encPath encContents) = liftIO $ do
      let fPath = myDecryptAES (aesPad sharedSeed) (encPath)
          fContents = myDecryptAES (aesPad sharedSeed) (encContents)
      let myFile = (Message fPath fContents)
      warnLog $ "Uploading file to db: [" ++ fPath ++ "]."
      putStrLn $ "REPLICATING A FILE HERE!!!"
      withMongoDbConnection $ upsert (select ["name" =: fPath] "FILE_RECORD") $ toBSON myFile
      return True

-- helper functions...
-- | Commits a list of files associated with a particular transaction to real db.
myCommit :: [Message] -> [FsAttributes] -> IO ()
myCommit [] _ = do
  warnLog $ "Nothing left to commit."
myCommit (entry@(Message fPath contents):rest) replicas = do
  withMongoDbConnection $ upsert (select ["name" =: fPath] "FILE_RECORD") $ toBSON entry
  broadcast entry replicas -- update replicas
  myCommit rest replicas

-- | Keeps the directory server informed about the file server's status.
stillAlive :: Int -> String -> IO ()
stillAlive wait port = do
  warnLog $ "Pinging directory server."
  h <- defaultHost
  let encHost = myEncryptAES (aesPad sharedSeed) h
      encPort = myEncryptAES (aesPad sharedSeed) port
  servDoCall (ping (Message encHost encPort)) dirPort
  threadDelay (wait * 1000000)
  stillAlive wait port -- recurse

-- | Passes file server's attributes to the directory server for registration.
iExist :: String -> IO ()
iExist port = do
  warnLog "Greeting directory server."
  myName <- fsName
  h <- defaultHost
  warnLog $ "This is my ip: " ++ h
  let encName = myEncryptAES (aesPad sharedSeed) myName
      encHost = myEncryptAES (aesPad sharedSeed) h
      encPort = myEncryptAES (aesPad sharedSeed) port
  resp <- servDoCall (registerFS (Message3 encName encHost encPort)) dirPort
  case resp of
    Left _ -> do
      warnLog $ "According to the directory server, I don't exist..."
    Right a -> do
      let s = "myStatus"
      case a of
        True -> do
          warnLog $ "I'm the primary!"
        otherwise -> do
          warnLog $ "I'm only a replica..."

-- | Broadcast changes to replica file servers.
broadcast :: Message -> [FsAttributes] -> IO ()
broadcast _ [] = warnLog $ "no replicas left for propagation"
broadcast (Message path contents) ((FsAttributes _ port):rs) = do
  let encPath = myEncryptAES (aesPad sharedSeed) (path)
      encContents = myEncryptAES (aesPad sharedSeed) (contents)
  let aFile = (Message encPath encContents)
  putStrLn $ "BROADCASTING TO: port = " ++ port
  servDoCall (UseHaskellAPIClient.replicateFile aFile) ((read port) :: Int)
  broadcast aFile rs

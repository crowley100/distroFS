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

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog $ "Starting transaction-service."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings -- port change?
  -- upsert global ID: 0
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
          :<|> readyCommit
          :<|> confirmCommit
  where
    beginTransaction :: StrWrap -> Handler ResponseData
    beginTransaction (StrWrap ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
          tRef = "globalID"
      warnLog $ "Client starting a new transaction."
      withMongoDbConnection $ do
        iD <- find (select ["name" =: tRef] "TID_RECORD") >>= drainCursor
        let newID = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) iD
        case newID of
          ((Message _ tID):_) -> liftIO $ do
            let retID = tID
                updatedID = (show ((read tID) + 1))
            let value = (Message tRef updatedID)
                encTID = myEncryptAES (aesPad seshKey) (retID)
            withMongoDbConnection $ upsert (select  ["name" =: tRef] "TID_RECORD") $ toBSON value
            withMongoDbConnection $ repsert (select  ["transID" =: retID] "TRANSACTION_RECORD") $ toBSON (Transaction retID [] [])
            return $ ResponseData encTID
          [] -> liftIO $ do
            let retID = "0"
                updatedID = "1"
            let value = (Message tRef updatedID)
                encTID = myEncryptAES (aesPad seshKey) (retID)
            withMongoDbConnection $ upsert (select  ["name" =: tRef] "TID_RECORD") $ toBSON value
            withMongoDbConnection $ repsert (select  ["transID" =: retID] "TRANSACTION_RECORD") $ toBSON (Transaction retID [] [])
            return $ ResponseData encTID

    tUpload :: FileTransaction -> Handler Bool
    tUpload (FileTransaction encTID (Modification (SendFileRef encFP encFDir encFID encTime encIP encPort) encText) ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let transID = myDecryptAES (aesPad seshKey) (encTID)
          fp = myDecryptAES (aesPad seshKey) (encFP)
          fdir = myDecryptAES (aesPad seshKey) (encFDir)
          fid = myDecryptAES (aesPad seshKey) (encFID)
          ts = myDecryptAES (aesPad seshKey) (encTime)
          ip = myDecryptAES (aesPad seshKey) (encIP)
          port = myDecryptAES (aesPad seshKey) (encPort)
          contents = myDecryptAES (aesPad seshKey) (encText)
      let change = (Modification (SendFileRef fp fdir fid ts ip port) contents)
      warnLog $ "Client uploading a modification to the transaction."
      let fsPath = (fdir ++ fid)
      withMongoDbConnection $ do
        findTrans <- find (select ["transID" =: transID] "TRANSACTION_RECORD") >>= drainCursor
        let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) findTrans
        case myTrans of
          ((Transaction someID changes paths):_) -> liftIO $ do
            let newT = Transaction someID (changes ++ [change]) (paths ++ [fsPath])
            withMongoDbConnection $ upsert (select  ["transID" =: someID] "TRANSACTION_RECORD") $ toBSON newT
            return True
          [] -> liftIO $ do
            return False

    commit :: Message -> Handler Bool
    commit (Message encTID ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let transID = myDecryptAES (aesPad seshKey) (encTID)
      warnLog $ "Client committing modifications in the transaction."
      -- initiate phase 2: talk to file servers
      withMongoDbConnection $ do
        findTrans <- find (select ["transID" =: transID] "TRANSACTION_RECORD") >>= drainCursor
        let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) findTrans
        case myTrans of
          ((Transaction someID changes _):_) -> liftIO $ do
            pushShadows someID changes
            return True
          [] -> return False

    abort :: Message -> Handler Bool
    abort (Message encTID ticket) = liftIO $ do
      let seshKey = myDecryptAES (aesPad sharedSeed) (ticket)
      let transID = myDecryptAES (aesPad seshKey) (encTID)
      warnLog $ "Client aborting modifications in the transaction."
      -- tell directory server
      toDir <- servDoCall (dirCommitShadow (Message transID "ticket")) dirPort
      case toDir of
        Left _ -> warnLog $ "service communication failure. (transaction -> dir server)"
        Right _ -> warnLog $ "service communication success. (transaction -> dir server)"
      withMongoDbConnection $ delete (select  ["transID" =: transID] "TRANSACTION_RECORD")
      return True

    readyCommit :: Message -> Handler Bool
    readyCommit (Message tID fPath) = liftIO $ do
      warnLog (tID ++ ": [" ++ fPath ++ "] ready to be committed.")
      withMongoDbConnection $ do
        findTrans <- find (select ["transID" =: tID] "TRANSACTION_RECORD") >>= drainCursor
        let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Transaction) findTrans
        case myTrans of
          ((Transaction someID changes paths):_) -> liftIO $ do
            let newPaths = readyUp paths [] fPath
            case newPaths of
              [] -> liftIO $ do
                warnLog $ "All servers ready to commit, broadcast response"
                -- broadcast to file servers: commit your shadow databases for tID
                broadcastCommit tID changes
                -- tell directory server to commit its shadow records too
                toDir <- servDoCall (dirCommitShadow (Message tID "ticket")) dirPort
                case toDir of
                  Left _ -> warnLog $ "service communication failure. (transaction -> dir server)"
                  Right _ -> warnLog $ "service communication success. (transaction -> dir server)"
                withMongoDbConnection $ delete (select  ["transID" =: tID] "TRANSACTION_RECORD")
              otherwise -> liftIO $ do
                warnLog $ "Ready to commit: " ++ fPath
            let newT = Transaction someID changes newPaths
            withMongoDbConnection $ upsert (select  ["transID" =: someID] "TRANSACTION_RECORD") $ toBSON newT
            return True
          [] -> liftIO $ do
            return False

    -- functionality to be determined...
    confirmCommit :: Message -> Handler Bool
    confirmCommit (Message tID fPath) = liftIO $ do
      warnLog (tID ++ ": [" ++ fPath ++ "] has been committed.")
      return True

-- helper functions
readyUp :: [String] -> [String] -> String -> [String]
readyUp (h:t) result fp | (h == fp) = (result ++ t)
                        | otherwise = readyUp t (result ++ [h]) fp
readyUp [] result _ = result

pushShadows :: String -> [Modification] -> IO ()
pushShadows _ [] = warnLog $ "No more changes to push."
pushShadows tID ((Modification (SendFileRef _ _ fID _ ip port) fContents):rest) = do
  let newShadow = (Shadow tID (Message fID fContents))
  toFS <- servDoCall (updateShadowDB newShadow) (read port)
  case toFS of
    Left _ -> warnLog $ "service communication failure. (transaction -> file server)"
    Right _ -> warnLog $ "serice communication success. (transaction -> file server)"
  pushShadows tID rest

broadcastCommit :: String -> [Modification] -> IO ()
broadcastCommit _ [] = warnLog $ "Finished broadcasting commits!"
broadcastCommit tID ((Modification (SendFileRef _ _ _ _ ip port) fContents):rest) = do
  toFS <- servDoCall (pushTransaction tID) (read port)
  case toFS of
    Left _ -> warnLog $ "service communication failure. (transaction -> file server)"
    Right _ -> warnLog $ "service communication success. (transaction -> file server)"
  broadcastCommit tID rest

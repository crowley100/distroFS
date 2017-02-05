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
import           System.Random

type MyDirAPI = "lsDir"               :> Get '[JSON] [FsContents]
           :<|> "lsFile"              :> QueryParam "name" String :> Get '[JSON] [FsContents]
           :<|> "fileQuery"           :> ReqBody '[JSON] Message :> Get '[JSON] [SendFileRef]
           :<|> "mapFile"             :> ReqBody '[JSON] Message :> Get '[JSON] [SendFileRef]
           :<|> "ping"                :> ReqBody '[JSON] Message :> Post '[JSON] Bool
           :<|> "registerFS"          :> ReqBody '[JSON] Message3 :> Post '[JSON] Bool
           :<|> "getPropagationInfo"  :> ReqBody '[JSON] Message :> Get '[JSON] [FsAttributes]
           :<|> "dirShadowing"        :> ReqBody '[JSON] Message3 :> Post '[JSON] [SendFileRef]
           :<|> "dirCommitShadow"     :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
           :<|> "dirAbortShadow"      :> ReqBody '[JSON] Message  :> Post '[JSON] Bool

-- some helper functions...

getDirInfo :: String -> IO [FsInfo]
getDirInfo fDir = liftIO $ do
  findFS <- withMongoDbConnection $ find (select ["myName" =: fDir] "FS_INFO") >>= drainCursor
  return (DL.take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsInfo) findFS)

-- elecet a replica to server as new primary server
primaryElection :: FsInfo -> FsInfo
primaryElection (FsInfo dName _ (r:rs)) = (FsInfo dName (Just r) rs)
primaryElection (FsInfo dName _ []) = (FsInfo dName Nothing []) -- all servers dead

-- check if any replicas aren't responding, remove those that aren't
updateReplicaStatus :: [FsAttributes] -> [FsAttributes] -> IO [FsAttributes]
updateReplicaStatus [] result = return result
updateReplicaStatus (r@(FsAttributes ip port):rest) result = liftIO $ do
  let rID = (ip ++ port)
  findStatus <- withMongoDbConnection $ find (select  ["name" =: rID] "FS_STATUS") >>= drainCursor
  let rStatus = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) findStatus
  case rStatus of
    ((Message _ tStamp):_) -> do
      currentTime <- getCurrentTime
      let myTime = (show currentTime)
      let tDiff = (cmpTime myTime tStamp)
      let isDead = tDiff > 10.0 -- some acceptable response delay
      case isDead of
        True -> updateReplicaStatus rest result
        otherwise -> updateReplicaStatus rest (result ++ [r])
    otherwise -> updateReplicaStatus rest result -- inconsitent state

-- Update server data structures to only contain responding servers
updateServerStatus :: String -> IO ()
updateServerStatus fDir = liftIO $ do
  servers <- getDirInfo fDir
  case servers of
    ((FsInfo dName p@(Just (FsAttributes ip port)) replicas):_) -> do
      newReplicas <- updateReplicaStatus replicas []
      let pID = (ip ++ port)
          newStatus = (FsInfo dName p newReplicas)
      findStatus <- withMongoDbConnection $ find (select  ["name" =: pID] "FS_STATUS") >>= drainCursor
      let pStatus = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) findStatus
      case pStatus of
        ((Message _ tStamp):_) -> do
          currentTime <- getCurrentTime
          let myTime = (show currentTime)
          let tDiff = (cmpTime myTime tStamp)
          let isDead = tDiff > 10.0 -- some acceptable response delay
          case isDead of
            True -> withMongoDbConnection $ upsert (select ["myName" =: fDir] "FS_INFO") $ toBSON (primaryElection newStatus)
            otherwise -> withMongoDbConnection $ upsert (select ["myName" =: fDir] "FS_INFO") $ toBSON newStatus
        otherwise -> withMongoDbConnection $ upsert (select ["myName" =: fDir] "FS_INFO") $ toBSON newStatus-- inconsitent state
    otherwise -> warnLog $ "error: inconsistent db state"

loadBalance :: [FsAttributes] -> IO FsAttributes
loadBalance [] = do
  return (FsAttributes "localhost" "8081")
loadBalance replicas = do
  let size = ((length replicas) - 1)
  index <- randomRIO (0, size)
  return (replicas !! index)

-- remove duplicate elements from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                     then seen
                                     else seen ++ [x]) []

-- if the file is a new addition to the directory, add it to contents
updateDirContents :: String -> String -> IO ()
updateDirContents fp fDir = do
  let fName = DL.drop (DL.length fDir) fp
  contents <- withMongoDbConnection $ find (select ["dirName" =: fDir] "CONTENTS_RECORD") >>= drainCursor
  let fList = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsContents) contents
  case fList of
    ((FsContents _ files):_) -> liftIO $ do
      let newFiles = removeDuplicates (files ++ [fName])
      let newContents = (FsContents fDir newFiles)
      withMongoDbConnection $ upsert (select  ["dirName" =: fDir] "CONTENTS_RECORD") $ toBSON newContents
    [] -> liftIO $ do warnLog $ "ERROR: inconsistent db record (CONTENTS_RECORD)"

-- push changes in shadow record to real db
doTheCommit :: [FileRef] -> IO ()
doTheCommit [] = putStrLn "Nothing left to push from shadow to real db..."
doTheCommit (ref@(FileRef fp dir _ _):rest) = do
  withMongoDbConnection $ upsert (select ["filePath" =: fp] "FILEREF_RECORD") $ toBSON ref
  updateDirContents fp dir

-- begin service
startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog $ "Starting directory-service."
  let settings = setPort 8000 $ setLogger aplogger defaultSettings -- port change?
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
        :<|> getPropagationInfo
        :<|> dirShadowing
        :<|> dirCommitShadow
        :<|> dirAbortShadow
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
      updateServerStatus fDir
      withMongoDbConnection $ do
        let filepath = (fDir ++ fName)
        findRef <- find (select ["filePath" =: filepath] "FILEREF_RECORD") >>= drainCursor
        let result = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) findRef
        case result of
          (ref@(FileRef fp dir fid fts):_) ->  liftIO $ do
            servers <- getDirInfo fDir
            case servers of
              [] -> return ([] :: [SendFileRef]) -- directory not found
              (fs@(FsInfo name (Just (FsAttributes newIp newPort)) []):_) -> do
                return [(SendFileRef fp dir fid fts newIp newPort)] -- no replicas, use primary
              (fs@(FsInfo name _ replicas):_) -> liftIO $ do
                (FsAttributes newIp newPort) <- loadBalance replicas
                return [(SendFileRef fp dir fid fts newIp newPort)]
          [] -> return ([] :: [SendFileRef]) -- file not found

    mapFile :: Message -> Handler [SendFileRef]
    mapFile val@(Message fName fDir) = liftIO $ do
      warnLog $ "Client mapping file ["++fName++"] in directory: " ++ fDir
      updateServerStatus fDir
      currentTime <- getCurrentTime
      let myTime = (show currentTime)
      let filepath = (fDir ++ fName)
      fs <- getDirInfo fDir
      case fs of
        [] -> return ([] :: [SendFileRef]) -- Can handle empty list on client side
        (info@(FsInfo _ (Just (FsAttributes ip port)) _):_) -> do
          docs <- withMongoDbConnection $ find (select ["filePath" =: filepath] "FILEREF_RECORD") >>= drainCursor
          let result = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) docs
          case result of
            ((FileRef somePath someDir someID t):_) -> do
              withMongoDbConnection $ upsert (select ["filePath" =: filepath] "FILEREF_RECORD") $ toBSON (FileRef somePath someDir someID myTime)
              return [(SendFileRef somePath someDir someID myTime ip port)]
            [] -> do
              -- update contents record
              contents <- withMongoDbConnection $ find (select ["dirName" =: fDir] "CONTENTS_RECORD") >>= drainCursor
              let fList = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FsContents) contents
              case fList of
                ((FsContents _ files):_) -> liftIO $ do
                  let newContents = (FsContents fDir (files ++ [fName]))
                  withMongoDbConnection $ upsert (select  ["dirName" =: fDir] "CONTENTS_RECORD") $ toBSON newContents
                [] -> liftIO $ do warnLog $ "ERROR: inconsistent db record (CONTENTS_RECORD)"
              ids <- withMongoDbConnection $ find (select ["directory" =: fDir] "ID_RECORD") >>= drainCursor
              let newID = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileID) ids
              case newID of
                ((FileID dirName myID):_) -> liftIO $ do
                  let retID = myID
                      updatedID = (show ((read myID) + 1))
                  let value = FileID dirName updatedID
                      result = (FileRef filepath fDir retID myTime)
                  withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                  withMongoDbConnection $ upsert (select  ["filePath" =: filepath] "FILEREF_RECORD") $ toBSON result
                  return [(SendFileRef filepath fDir retID myTime ip port)]
                [] -> liftIO $ do
                  let retID = "0"
                      updatedID = "1"
                  let value = FileID fDir updatedID
                      result = (FileRef filepath fDir retID myTime)
                  withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                  withMongoDbConnection $ upsert (select  ["filePath" =: filepath] "FILEREF_RECORD") $ toBSON result
                  return [(SendFileRef filepath fDir retID myTime ip port)]

    -- confirm fs is still alive
    ping :: Message -> Handler Bool
    ping msg@(Message ip port) = liftIO $ do
      currentTime <- getCurrentTime
      let myTime = (show currentTime)
      let fsID = (ip ++ port)
      let updatedStatus = (Message fsID myTime)
      withMongoDbConnection $ upsert (select  ["name" =: fsID] "FS_STATUS") $ toBSON updatedStatus
      return True

    -- register a file server
    registerFS :: Message3 -> Handler Bool
    registerFS msg3@(Message3 dirName fsIP fsPort) = liftIO $ do
      warnLog $ "Registering file server associated with [" ++ dirName ++ "]"
      fs <- getDirInfo dirName
      case fs of
        [] -> do
          let newEntry = (FsInfo dirName (Just (FsAttributes fsIP fsPort)) [])
          withMongoDbConnection $ upsert (select ["myName" =: dirName] "FS_INFO") $ toBSON newEntry
          return True
        ((FsInfo _ primary replicas):_) -> do
          case primary of
            (Just pServer) -> do
              let newReplica = (FsAttributes fsIP fsPort)
              let newEntry = (FsInfo dirName (Just pServer) (replicas ++ [newReplica]))
              withMongoDbConnection $ upsert (select ["myName" =: dirName] "FS_INFO") $ toBSON newEntry
              return True
            otherwise -> do -- handle edge case of no primary server
              let newPrimary = Just (FsAttributes fsIP fsPort)
              let newEntry = (FsInfo dirName newPrimary replicas)
              withMongoDbConnection $ upsert (select ["myName" =: dirName] "FS_INFO") $ toBSON newEntry
              return True

    getPropagationInfo :: Message -> Handler [FsAttributes]
    getPropagationInfo (Message directory ticket) = liftIO $ do
      serverInfo <- getDirInfo directory
      case serverInfo of
        [] -> return ([] :: [FsAttributes])
        ((FsInfo _ _ replicas):_) -> return replicas

    dirShadowing :: Message3 -> Handler [SendFileRef]
    dirShadowing (Message3 tID fName fDir) = liftIO $ do
      warnLog $ "Shadowing ["++fName++"] in directory: " ++ fDir
      updateServerStatus fDir
      currentTime <- getCurrentTime
      let myTime = (show currentTime)
      let filepath = (fDir ++ fName)
      fs <- getDirInfo fDir
      case fs of
        [] -> return ([] :: [SendFileRef]) -- Can handle empty list on client side
        (info@(FsInfo _ (Just (FsAttributes ip port)) _):_) -> do
          docs <- withMongoDbConnection $ find (select ["filePath" =: filepath] "FILEREF_RECORD") >>= drainCursor
          let result = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) docs
          case result of
            ((FileRef somePath someDir someID t):_) -> do
              findShadows <- withMongoDbConnection $ find (select ["dTID" =: tID] "SHADOW_REFS") >>= drainCursor
              let shadowRefs = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowRef) findShadows
              case shadowRefs of
                (entry@(ShadowRef _ refs):_) -> do -- add to transaction in progress
                  let newRefs = refs ++ [(FileRef somePath someDir someID myTime)]
                  let newEntry = (ShadowRef tID newRefs)
                  withMongoDbConnection $ upsert (select ["dTID" =: tID] "SHADOW_REFS") $ toBSON newEntry
                otherwise -> do -- new transaction
                  let newEntry = (ShadowRef tID [(FileRef somePath someDir someID myTime)])
                  withMongoDbConnection $ upsert (select ["dTID" =: tID] "SHADOW_REFS") $ toBSON newEntry
              return [(SendFileRef somePath someDir someID myTime ip port)]
            [] -> do
              -- no need to shadow ids
              ids <- withMongoDbConnection $ find (select ["directory" =: fDir] "ID_RECORD") >>= drainCursor
              let newID = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileID) ids
              case newID of
                ((FileID dirName myID):_) -> liftIO $ do
                  let retID = myID
                      updatedID = (show ((read myID) + 1))
                  let value = FileID dirName updatedID
                      result = (FileRef filepath fDir retID myTime)
                  findShadows <- withMongoDbConnection $ find (select ["dTID" =: tID] "SHADOW_REFS") >>= drainCursor
                  let shadowRefs = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowRef) findShadows
                  case shadowRefs of
                    (entry@(ShadowRef _ refs):_) -> do
                      let newEntry = (ShadowRef tID (refs ++ [result]))
                      withMongoDbConnection $ upsert (select  ["dTID" =: filepath] "SHADOW_REFS") $ toBSON newEntry
                    otherwise -> do
                      let newEntry = (ShadowRef tID [result])
                      withMongoDbConnection $ upsert (select  ["filePath" =: filepath] "SHADOW_REFS") $ toBSON newEntry
                  withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                  return [(SendFileRef filepath fDir retID myTime ip port)]
                [] -> liftIO $ do
                  let retID = "0"
                      updatedID = "1"
                  let value = FileID fDir updatedID
                      result = (FileRef filepath fDir retID myTime)
                  findShadows <- withMongoDbConnection $ find (select ["dTID" =: tID] "SHADOW_REFS") >>= drainCursor
                  let shadowRefs = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowRef) findShadows
                  case shadowRefs of
                    (entry@(ShadowRef _ refs):_) -> do
                      let newEntry = (ShadowRef tID (refs ++ [result]))
                      withMongoDbConnection $ upsert (select  ["dTID" =: tID] "SHADOW_REFS") $ toBSON newEntry
                    otherwise -> do
                      let newEntry = (ShadowRef tID [result])
                      withMongoDbConnection $ upsert (select  ["dTID" =: tID] "SHADOW_REFS") $ toBSON newEntry
                  withMongoDbConnection $ upsert (select  ["directory" =: fDir] "ID_RECORD") $ toBSON value
                  return [(SendFileRef filepath fDir retID myTime ip port)]

    -- maybe dont use ticket below as it is server-server comms
    dirCommitShadow :: Message -> Handler Bool
    dirCommitShadow (Message tID ticket) = liftIO $ do
      findShadows <- withMongoDbConnection $ find (select ["dTID" =: tID] "SHADOW_REFS") >>= drainCursor
      let shadowRefs = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ShadowRef) findShadows
      case shadowRefs of
        (shadow@(ShadowRef _ refs):_) -> do
          doTheCommit refs
          return True
        otherwise -> return False

    dirAbortShadow :: Message -> Handler Bool
    dirAbortShadow (Message tID ticket) = liftIO $ do
      withMongoDbConnection $ delete (select  ["dTID" =: tID] "SHADOW_REFS")
      return True

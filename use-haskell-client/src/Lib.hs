{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad                      (join, when)
import           Control.Monad.IO.Class
import qualified Data.List                          as DL
import           Distribution.PackageDescription.TH
import           Git.Embed
import           System.IO
import           Data.Maybe
import           Control.Monad.Trans.Resource
import           Data.Bson.Generic
import           GHC.Generics
import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Options.Applicative
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import           System.Console.ANSI
import           System.Environment
import           UseHaskellAPI
import           UseHaskellAPIClient
import           UseHaskellAPITypes
import           RSAhelpers
import           Data.Text (pack, unpack)
import           Database.MongoDB
import           Data.Time.Clock
import           Data.Time.Format

gitRev, gitBranch, cabalAuthor, cabalVersion, cabalCopyright :: String
gitRev = $(embedGitShortRevision)
gitBranch = $(embedGitBranch)
cabalAuthor = $( packageVariable  (packageString . author))
cabalVersion = $( packageVariable (pkgVersion . package))
cabalCopyright = $( packageVariable (packageString . copyright ))

-- | helper functions to change color in ansi terminal output (mor for the fun of it)
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
resetCode = setSGRCode [Reset]

-- | output a command line banner
banner :: IO ()
banner = do
  progName <- getProgName
  putStrLn $ "\n" ++ redCode ++ progName ++ " (" ++ cabalVersion ++ ")  - " ++
             cabalCopyright ++" (" ++ cabalAuthor ++ ")" ++ resetCode ++ "\n" ++
             whiteCode ++ "Git Branch: " ++ gitBranch ++ ", Git Revision: " ++ gitRev ++ resetCode ++ "\n"

-- | A helper function to make it easier to execute rest calls and report errors if the occur
reportExceptionOr act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''

-- | a simple handler class to print the response. We can pass this as the first parameter of calls to
-- reportOrException. It will call the appropriate version depending on the type of the returned results.

class PrintResponse a where
  resp :: Show a => a -> String

instance PrintResponse ResponseData where
  resp r = "Response is a single value: " ++ response r

instance PrintResponse [Message] where
  resp [] = "No messages."
  resp [x] = "Response is a single message: " ++ message x
  resp rs = "Response is an array with messages: " ++ (DL.intercalate ", " $ DL.map message rs)

instance PrintResponse [ResponseData] where
  resp rs = "Response is an array with values: " ++ (DL.intercalate ", " $ DL.map response rs)

instance PrintResponse Bool where
  resp True =  "Response is a boolean : Totally!"
  resp False = "Response is a boolean : Like No Way!"

instance PrintResponse FsContents where
  resp r = "Response is an array with values: " ++ dirName r

instance PrintResponse [FsContents] where
  resp [] = "end."
  resp [x] = "Response is a single message: " ++ dirName x
  resp rs = "Response is an array with values: " ++ (DL.intercalate ", " $ DL.map dirName rs)

-- | Command line option handlers, one for each command
-- These are called from the options parsing and do the actuall work of the program.

-- let's put all the hard work in a helper...
doCall f h p = reportExceptionOr (putStrLn . resp) (SC.runClientM f =<< env h p)

-- another helper that allows us to use server response
myDoCall f h p = (SC.runClientM f =<< env h p)

-- | Initialises communication with the system for a new user
--  by talking to the authentication server.
doSignUp :: String -> String -> Maybe String -> Maybe String -> IO ()
doSignUp name pass host port = do
   resp <- myDoCall (loadPublicKey) host (Just (show authPort))
   case resp of
     Left err -> do
       putStrLn "failed to get public key..."
     Right ((ResponseData a):(ResponseData b):(ResponseData c):rest) -> do
       let authKey = toPublicKey (PubKeyInfo a b c)
       cryptPass <- encryptPass authKey pass
       putStrLn "got the public key!"
       doCall (signUp $ Login name cryptPass) host (Just (show authPort))

-- | Authentication server provides the client with communication credentials.
doLogIn :: String -> String -> Maybe String -> Maybe String -> IO ()
doLogIn name pass host port = do
  resp <- myDoCall (loadPublicKey) host (Just (show authPort))
  case resp of
    Left err -> do
      putStrLn "failed to get public key..."
    Right ((ResponseData a):(ResponseData b):(ResponseData c):rest) -> do
      let authKey = toPublicKey (PubKeyInfo a b c)
      cryptPass <- encryptPass authKey pass
      putStrLn "got the public key!"
      details <- myDoCall (logIn $ Login name cryptPass) host (Just (show authPort))
      case details of
        Left err -> do
          putStrLn "login failure..."
        Right ((ResponseData ticket):(ResponseData encSesh):(ResponseData encExpiryDate):_) -> do
          let key = "MyDetails" :: String
              mySesh = myDecryptAES (aesPad pass) (encSesh)
              expiryDate = myDecryptAES (aesPad pass) (encExpiryDate)
          let myDetails = (Details key name mySesh ticket expiryDate) -- date doesn't require decryption
          withClientMongoDbConnection $ repsert (select ["clientKey" =: key] "DETAILS_RECORD") $ toBSON myDetails
          putStrLn "login success!"

-- ls commands giving the client a view of the file system
-- | Lists file servers acting as directories to the client.
doLsDir :: Maybe String -> Maybe String -> IO ()
doLsDir h p = do
  myDetails <- authenticateUser
  case myDetails of
    (Just (Details _ _ mySesh myTicket _)) -> do
      getDirs <- myDoCall (lsDir (StrWrap myTicket)) h (Just (show dirPort))
      case getDirs of
        Left err -> putStrLn $ "error listing directories: " ++ (show err)
        Right (ResponseData encDirs) -> putStrLn $ myDecryptAES (aesPad mySesh) encDirs
    otherwise -> putStrLn $ "DENIED: Invalid session, please log in."

-- | Lists contents of a particular directory.
doLsFile :: String -> Maybe String -> Maybe String -> IO ()
doLsFile dirName h p = do
  myDetails <- authenticateUser
  case myDetails of
    (Just (Details _ _ mySesh myTicket _)) -> do
      let encDir = myEncryptAES (aesPad mySesh) dirName
      getFiles <- myDoCall (lsFile $ Message encDir myTicket) h (Just (show dirPort))
      case getFiles of
        Left err -> putStrLn $ "error listing files: " ++ (show err)
        Right ((FsContents _ files):_) -> do
          case files of
            [] -> do putStrLn "directory empty..."
            [x] -> do putStrLn $ "Files: " ++ (myDecryptAES (aesPad mySesh) x)
            xs -> do putStrLn $ "Files: " ++ (DL.intercalate "\n " $ DL.map (myDecryptAES (aesPad mySesh)) xs)
    otherwise -> putStrLn $ "DENIED: Invalid session, please log in."

-- | File download, client asks directory server for directory / file communication
--  details, client then uses these details to request the file. Load balancing is employed
--  to reduce stress on the file server by directing these queries to replicas.
doFileQuery :: String -> String -> Maybe String -> Maybe String -> IO ()
doFileQuery fileName dirName h p = do
  myDetails <- authenticateUser
  case myDetails of
    (Just (Details _ _ mySesh myTicket _)) -> do
      let encFileName = myEncryptAES (aesPad mySesh) fileName
          encDir = myEncryptAES (aesPad mySesh) dirName
      getRef <- myDoCall (fileQuery $ Message3 encFileName encDir myTicket) h (Just (show dirPort))
      case getRef of
        Left err -> do
          putStrLn $ "error [" ++ (show err) ++ "] querying file..."
        Right response -> do
          case response of
            ((SendFileRef encFPath _ encFID encTime encIP encPort):_) -> do
              let fPath = myDecryptAES (aesPad mySesh) (encFPath)
                  fID = myDecryptAES (aesPad mySesh) (encFID)
                  myTime = myDecryptAES (aesPad mySesh) (encTime)
                  fsIP = myDecryptAES (aesPad mySesh) (encIP)
                  fsPort = myDecryptAES (aesPad mySesh) (encPort)
              putStrLn ("ID: " ++ fID ++ "\nIP: " ++ fsIP ++ "\nPort: " ++ fsPort)
              checkTimeStamp <- notCached myTime fPath
              case checkTimeStamp of
                True -> do
                  getFile <- myDoCall (download $ Message encFID myTicket) (Just fsIP) (Just fsPort)
                  case getFile of
                    Left err -> do
                      putStrLn "error retrieving file..."
                    Right ((Message _ encText):_) -> do
                      let cacheRef = (FileRef fPath dirName fID myTime) -- cache file
                          text = myDecryptAES (aesPad mySesh) (encText)
                      withClientMongoDbConnection $ upsert (select ["fp" =: fPath] "CLIENT_CACHE") $ toBSON cacheRef
                      writeFile fileName text
                otherwise -> putStrLn $ "Using cached: " ++ dirName ++ "/" ++ fileName
            [] -> do
              putStrLn (fileName ++ " does not exist in " ++ dirName)
    otherwise -> putStrLn $ "DENIED: Invalid session, please log in."

-- | File upload, client asks directory server for directory / file communication
--  details, client then uses these details to push the file, provided the client can aquire the lock.
doMapFile :: String -> String -> Maybe String -> Maybe String -> IO ()
doMapFile fileName dirName h p = do
  let filePath = (dirName ++ fileName)
  myDetails <- authenticateUser
  case myDetails of
    (Just (Details _ myName mySesh myTicket _)) -> do
      let encFp = myEncryptAES (aesPad mySesh) (filePath)
          encName = myEncryptAES (aesPad mySesh) (myName)
          encFName = myEncryptAES (aesPad mySesh) (fileName)
          encDirName = myEncryptAES (aesPad mySesh) (dirName)
      tryGetLock <- myDoCall (lock $ Message3 encFp encName myTicket) h (Just (show lockPort))
      case tryGetLock of -- try to lock file for writing
        Right True -> do
          -- check if transaction in progress
          let owner = "clientTransaction" :: String
          findTrans <- withClientMongoDbConnection $ find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
          let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
          case myTrans of
            ((CurrentTrans _ tID):_) -> do
              transactionLock tID filePath -- keep track of files locked for this transaction
              let encTID = myEncryptAES (aesPad mySesh) (tID)
              getMapping <- myDoCall (dirShadowing $ Message4 encTID encFName encDirName myTicket) h (Just (show dirPort))
              case getMapping of
                Left err -> do
                  putStrLn $ "error [" ++ (show err) ++ "] mapping file..."
                Right response -> do
                  case response of
                    [] -> putStrLn $ "No such filepath: " ++ dirName ++ "/" ++ fileName
                    (ref:_) -> do
                      contents <- readFile fileName
                      let encText = myEncryptAES (aesPad mySesh) (contents)
                      putStrLn "Pushing modification to transaction server..."
                      let update = (Modification ref encText)
                      let fileT = (FileTransaction encTID update myTicket)
                      doCall (tUpload fileT) h (Just (show transPort))
            otherwise -> do
              getMapping <- myDoCall (mapFile $ Message3 encFName encDirName myTicket) h (Just (show dirPort))
              case getMapping of
                Left err -> do
                  putStrLn $ "error [" ++ (show err) ++ "] mapping file..."
                Right response -> do
                  case response of
                    [] -> putStrLn $ "No such filepath: " ++ dirName ++ "/" ++ fileName
                    (ref@(SendFileRef _ _ encFID _ encIP encPort):_) -> do
                      let fsIP = myDecryptAES (aesPad mySesh) (encIP)
                          fsPort = myDecryptAES (aesPad mySesh) (encPort)
                      contents <- readFile fileName
                      let encText = myEncryptAES (aesPad mySesh) (contents)
                      putStrLn "Uploading file to file server..."
                      doCall (upload $ Message3 encFID encText myTicket) (Just fsIP) (Just fsPort)
                      -- unlocking only required here if upload is not part of transaction
                      doCall (unlock $ Message3 encFp encName myTicket) h (Just (show lockPort))
        otherwise -> putStrLn $ "DENIED: " ++ dirName ++ "/" ++ fileName ++ " is locked, try again later..."
    otherwise -> putStrLn $ "DENIED: Invalid session, please log in."

-- | Client retrieves a transaction ID from the transaction service
--  all subsequent uploads will be part of this transaction until client commits/aborts.
doBeginTrans :: Maybe String -> Maybe String -> IO ()
doBeginTrans h p = do
  myDetails <- authenticateUser
  case myDetails of
    (Just (Details _ myName mySesh myTicket _)) -> do
      getTransID <- myDoCall (beginTransaction (StrWrap myTicket)) h (Just (show transPort))
      let owner = "clientTransaction" :: String
      case getTransID of
        Left err -> do
          putStrLn "error obtaining transaction ID..."
        Right (ResponseData encTID) -> do
          let tID = myDecryptAES (aesPad mySesh) (encTID)
          putStrLn ("New transaction ID: " ++ tID)
          -- store tID in client db
          withClientMongoDbConnection $ do
            findTrans <- find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
            let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
            case myTrans of
              ((CurrentTrans _ oldTID):_) -> liftIO $ do
                putStrLn "Aborting current transaction and starting new one..."
                let encOldTID = myEncryptAES (aesPad mySesh) oldTID
                doCall (abort (Message encOldTID myTicket)) h (Just (show transPort)) -- tell transaction server to abort previous transaction
                transactionUnlock oldTID myName mySesh myTicket -- unlock all files in transaction
                withClientMongoDbConnection $ repsert (select ["tOwner" =: owner] "MY_TID") $ toBSON (CurrentTrans owner tID)
              [] -> liftIO $ do
                putStrLn "starting new transaction..."
                withClientMongoDbConnection $ upsert (select ["tOwner" =: owner] "MY_TID") $ toBSON (CurrentTrans owner tID)
    otherwise -> putStrLn $ "DENIED: Invalid session, please log in."

-- | Fetch tID from client db, talk to transaction server to proceed with transaction, remove ID from db.
doCommit :: Maybe String -> Maybe String -> IO ()
doCommit h p = do
  myDetails <- authenticateUser
  case myDetails of
    (Just (Details _ myName mySesh myTicket _)) -> do
      let owner = "clientTransaction" :: String
      withClientMongoDbConnection $ do
        findTrans <- find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
        let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
        case myTrans of
          ((CurrentTrans _ tID):_) -> liftIO $ do
            putStrLn "Committing current transaction..."
            let encTID = myEncryptAES (aesPad mySesh) (tID)
            doCall (commit (Message encTID myTicket)) h (Just (show transPort))
            withClientMongoDbConnection $ delete (select ["tOwner" =: owner] "MY_TID")
            transactionUnlock tID myName mySesh myTicket -- unlock all files in transaction
          [] -> liftIO $ do
            putStrLn "No active transactions..."
    otherwise -> putStrLn $ "DENIED: Invalid session, please log in."

-- | Fetch tID from client db, talk to transaction server to abort transaction, remove ID from db.
doAbort :: Maybe String -> Maybe String -> IO ()
doAbort h p = do
  myDetails <- authenticateUser
  case myDetails of
    (Just (Details _ myName mySesh myTicket _)) -> do
      let owner = "clientTransaction" :: String
      withClientMongoDbConnection $ do
        findTrans <- find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
        let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
        case myTrans of
          ((CurrentTrans _ tID):_) -> liftIO $ do
            putStrLn "Aborting current transaction..."
            let encTID = myEncryptAES (aesPad mySesh) (tID)
            doCall (abort (Message encTID myTicket)) h (Just (show transPort))
            withClientMongoDbConnection $ delete (select ["tOwner" =: owner] "MY_TID")
            transactionUnlock tID myName mySesh myTicket -- unlock all files in transaction
          [] -> liftIO $ do
            putStrLn "No active transactions..."
    otherwise -> putStrLn $ "DENIED: Invalid session, please log in."

-- | The options handling

-- First we invoke the options on the entry point.
someFunc :: IO ()
someFunc = do
  banner
  join $ execParser =<< opts

-- | Defined in the applicative style, opts provides a declaration of the entire command line
--   parser structure.
opts :: IO (ParserInfo (IO ()))
opts = do
  progName <- getProgName
  return $ info (   helper
                <*> subparser
                       (  command "sign-up"
                                   (withInfo ( doSignUp
                                           <$> argument str (metavar "UserName")
                                           <*> argument str (metavar "Password")
                                           <*> serverIpOption
                                           <*> serverPortOption) "Sign user up to the remote server." )
                       <> command "log-in"
                                   (withInfo ( doLogIn
                                           <$> argument str (metavar "UserName")
                                           <*> argument str (metavar "Password")
                                           <*> serverIpOption
                                           <*> serverPortOption) "Logs user into the remote server." )
                       <> command "ls-dir"
                                   (withInfo ( doLsDir
                                            <$> serverIpOption
                                            <*> serverPortOption) "List directories in the file system." )
                       <> command "ls-file"
                                   (withInfo ( doLsFile
                                            <$> argument str (metavar "fDir")
                                            <*> serverIpOption
                                            <*> serverPortOption) "List files contained in a directory." )
                       <> command "download"
                                   (withInfo ( doFileQuery
                                            <$> argument str (metavar "fPath")
                                            <*> argument str (metavar "fDir")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Init download communication." )
                       <> command "upload"
                                   (withInfo ( doMapFile
                                            <$> argument str (metavar "fPath")
                                            <*> argument str (metavar "fDir")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Init upload communication." )
                       <> command "begin-transaction"
                                   (withInfo ( doBeginTrans
                                            <$> serverIpOption
                                            <*> serverPortOption) "Start a new transaction." )
                       <> command "commit"
                                   (withInfo ( doCommit
                                            <$> serverIpOption
                                            <*> serverPortOption) "Commit current transaction." )
                       <> command "abort"
                                   (withInfo ( doAbort
                                            <$> serverIpOption
                                            <*> serverPortOption) "Abort current transaction." )))
             (  fullDesc
             <> progDesc (progName ++ " is a simple client proxy for the distroFS file system." ++
                          " Try " ++ whiteCode ++ progName ++ " --help " ++ resetCode ++ " for more information. To " ++
                          " see the details of any command, " ++  "try " ++ whiteCode ++ progName ++ " COMMAND --help" ++
                          resetCode ++ ". The application supports bash completion. To enable, " ++
                          "ensure you have bash-completion installed and enabled (see your OS for details), the " ++
                          whiteCode ++ progName ++ resetCode ++
                          " application in your PATH, and place the following in your ~/.bash_profile : " ++ whiteCode ++
                          "source < (" ++ progName ++ " --bash-completion-script `which " ++ progName ++ "`)" ++
                          resetCode )
             <> header  (redCode ++ "Git revision : " ++ gitRev ++ ", branch: " ++ gitBranch ++ resetCode))

-- Transaction locking helpers, with authentication for unlock calls, expiry date checked by caller
-- | Add fp to list of files locked for a particular transaction
transactionLock :: String -> String -> IO ()
transactionLock tID fp = liftIO $ do
  findTrans <- withClientMongoDbConnection $ find (select ["tLocksID" =: tID] "TLOCKS_RECORD") >>= drainCursor
  let tLocks = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransLocks) findTrans
  case tLocks of
    ((TransLocks _ lockedPaths):_) ->
      withClientMongoDbConnection $ upsert (select ["tLocksID" =: tID] "TLOCKS_RECORD") $ toBSON (TransLocks tID (lockedPaths ++ [fp]))
    otherwise ->
      withClientMongoDbConnection $ upsert (select ["tLocksID" =: tID] "TLOCKS_RECORD") $ toBSON (TransLocks tID [fp])

-- | Unlock all files associated with a particular transaction
transactionUnlock :: String -> String -> String -> String -> IO ()
transactionUnlock tID uName seshKey ticket = liftIO $ do
  let encName = myEncryptAES (aesPad seshKey) (uName)
  findTrans <- withClientMongoDbConnection $ find (select ["tLocksID" =: tID] "TLOCKS_RECORD") >>= drainCursor
  let tLocks = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe TransLocks) findTrans
  case tLocks of
    ((TransLocks _ lockedPaths):_) -> do
      recursiveUnlock encName lockedPaths seshKey ticket
      withClientMongoDbConnection $ delete (select ["tLocksID" =: tID] "TLOCKS_RECORD")
    otherwise ->
      return () -- edge case

-- | do unlock operations on a list of files
recursiveUnlock :: String -> [String] -> String -> String -> IO ()
recursiveUnlock _ [] _ _ = putStrLn $ "Finished transaction unlocks."
recursiveUnlock encName (p:ps) seshKey ticket = do
  let encFP = myEncryptAES (aesPad seshKey) (p)
  doCall (unlock (Message3 encFP encName ticket)) Nothing (Just (show lockPort))
  recursiveUnlock encName ps seshKey ticket

-- Extract my details
getDetails :: IO [Details]
getDetails = do
  let key = "MyDetails" :: String
  findDetails <- withClientMongoDbConnection $ find (select ["clientKey" =: key] "DETAILS_RECORD") >>= drainCursor
  return (catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Details) findDetails)

-- Check if session key has expired
validSession :: String -> IO Bool
validSession expiryDate = do
  time <- getCurrentTime
  return ((cmpTime expiryDate (show time)) > 0.0)

-- | Checks if the user has been authenticated, and that their session hasn't expired.
-- Returns user's credentials for further communication with services.
authenticateUser :: IO (Maybe Details)
authenticateUser = do
  let key = "MyDetails" :: String
  findDetails <- withClientMongoDbConnection $ find (select ["clientKey" =: key] "DETAILS_RECORD") >>= drainCursor
  let myDetails = (catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Details) findDetails)
  case myDetails of
    (retVal@(Details _ myName mySesh myTicket myExpiryDate):_) -> do
      -- check if session key has expired
      time <- getCurrentTime
      let notExpired = ((cmpTime myExpiryDate (show time)) > 0.0)
      case notExpired of
        True -> return (Just retVal)
        otherwise -> return Nothing
    otherwise -> return Nothing

-- MongoDB client info
withClientMongoDbConnection :: Action IO a -> IO a
withClientMongoDbConnection act  = do
  let port = 27017
      database = "USEHASKELLDB"
  pipe <- connect (host "127.0.0.1")
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

-- caching helper
notCached :: String -> String -> IO Bool
notCached remoteTime fp = do
  putStrLn "Checking cache..."
  checkCache <- withClientMongoDbConnection $ find (select ["fp" =: fp] "CLIENT_CACHE") >>= drainCursor
  let cache = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe FileRef) checkCache
  case cache of
    [] -> return True -- file not in cache
    ((FileRef _ _ _ localTime):_) -> return ((cmpTime remoteTime localTime) > 0)

-- helpers to simplify the creation of command line options
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


serverIpOption :: Parser (Maybe String)
serverIpOption = optional $ strOption ( long "ip"
                                     <> short 'i'
                                     <> metavar "IPADDRESS"
                                     <> help "the ip address of the use-haskell service.")

serverPortOption :: Parser (Maybe String)
serverPortOption = optional $ strOption (  long "port"
                                        <> short 'n'
                                        <> metavar "PORT_NUMBER"
                                        <> help "The port number of the use-haskell service.")



-- | function to build the client environment for performing a servant client rest call
-- It uses the host name and port parameters if Just x, or else uses envrionment variables

env :: Maybe String -> Maybe String -> IO SC.ClientEnv
env host port = SC.ClientEnv <$> newManager defaultManagerSettings
                                               <*> (SC.BaseUrl <$> pure SC.Http
                                                               <*> (host <?> usehaskellHost)
                                                               <*> (read <$> (port <?> usehaskellPort))
                                                               <*> pure "")
 where
   (<?>) :: Maybe a -> IO a -> IO a
   h <?> f = case h of
     Just hst -> return hst
     Nothing  -> f

   -- | The url endpoint for contactingt the use-haskell service
   usehaskellHost :: IO String
   usehaskellHost = devEnv "USE_HASKELL_HOST" id "localhost" True

   -- | The neo4j port
   usehaskellPort :: IO String
   usehaskellPort = devEnv "USE_HASKELL_PORT" id "8080" True


   -- | Helper function to simplify the setting of environment variables
   -- function that looks up environment variable and returns the result of running funtion fn over it
   -- or if the environment variable does not exist, returns the value def. The function will optionally log a
   -- warning based on Boolean tag

   devEnv :: Show a
          => String        -- Environment Variable name
          -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
          -> a             -- default value to use if environment variable is not set
          -> Bool          -- True if we should warn if environment variable is not set
          -> IO a
   devEnv env fn def warn = lookupEnv env >>= \ result ->
     case result of
         Just s  -> return $ fn s
         Nothing -> warn' warn env def

    where warn' :: Show b => Bool -> String -> b -> IO b
          warn' wn e s =  do
            when wn $ putStrLn $ "Environment variable: " ++ e ++
                                    " is not set. Defaulting to " ++ (show s)
            return s

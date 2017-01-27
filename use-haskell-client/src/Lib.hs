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
import           RSAhelpers
import           Data.Text (pack, unpack)
import           Database.MongoDB

-- | to embed git and cabal details for this build into the executable (just for the fun of it)
-- The code inside $( ) gets run at compile time. The functions run extract data from project files, both .git files and
-- the .cabal file.

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

myDoCall f h p = (SC.runClientM f =<< env h p)

-- which makes the actual rest calls trivial...(notice the currying)

doLoadEnvVars :: Maybe String -> Maybe String -> Maybe String -> IO ()
doLoadEnvVars s = doCall $ loadEnvVars s

doGetREADME :: Maybe String -> Maybe String -> IO ()
doGetREADME  = doCall getREADME

doStoreMessage :: String -> String -> Maybe String -> Maybe String -> IO ()
doStoreMessage n m  = doCall $ storeMessage $ Message n m

doSignUp :: String -> String -> Maybe String -> Maybe String -> IO ()
doSignUp name pass host port = do
   resp <- myDoCall (loadPublicKey) host port
   case resp of
     Left err -> do
       putStrLn "failed to get public key..."
     Right ((ResponseData a):(ResponseData b):(ResponseData c):rest) -> do
       let authKey = toPublicKey (PubKeyInfo a b c)
       cryptPass <- encryptPass authKey pass
       putStrLn "got the public key!"
       doCall (signUp $ Login name cryptPass) host port

doLogIn :: String -> String -> Maybe String -> Maybe String -> IO () -- return token here instead ?
doLogIn name pass host port = do
  resp <- myDoCall (loadPublicKey) host port
  case resp of
    Left err -> do
      putStrLn "failed to get public key..."
    Right ((ResponseData a):(ResponseData b):(ResponseData c):rest) -> do
      let authKey = toPublicKey (PubKeyInfo a b c)
      cryptPass <- encryptPass authKey pass
      putStrLn "got the public key!"
      doCall (logIn $ Login name cryptPass) host port

doSearchMessage :: String -> Maybe String -> Maybe String -> IO ()
doSearchMessage s  = doCall $ searchMessage $ Just s

doPerformRestCall :: Maybe String -> Maybe String -> Maybe String -> IO ()
doPerformRestCall s  =  doCall $ performRestCall s

-- lock service commands
doLockFile :: String -> Maybe String -> Maybe String -> IO ()
doLockFile fName = doCall $ lock fName

doUnlockFile :: String -> Maybe String -> Maybe String -> IO ()
doUnlockFile fName = doCall $ unlock fName

doFileLocked :: String -> Maybe String -> Maybe String -> IO ()
doFileLocked fName = doCall $ locked $ Just fName

-- file service commands
doDownloadFile :: String -> Maybe String -> Maybe String -> IO ()
doDownloadFile fPath h p = do
  getFile <- myDoCall (download $ Just fPath) h p
  case getFile of
    Left err -> do
      putStrLn "error retrieving file..."
    Right ((Message file_path text):rest) ->
      writeFile file_path text

doUploadFile :: String -> Maybe String -> Maybe String -> IO ()
doUploadFile fPath h p = do
  let owner = "clientTransaction" :: String
  contents <- readFile fPath
  -- check if transaction in progress
  withClientMongoDbConnection $ do
    findTrans <- find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
    let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
    case myTrans of
      ((CurrentTrans _ tID):_) -> liftIO $ do
        putStrLn "Pushing modification to transaction server..."
        
      [] -> liftIO $ do
        putStrLn "Uploading file to file server..."
        doCall (upload $  Message fPath contents) h p

-- directory service commands
doLsDir :: Maybe String -> Maybe String -> IO ()
doLsDir h p = do
   doCall lsDir h (Just "8000")

doLsFile :: String -> Maybe String -> Maybe String -> IO ()
doLsFile dirName h p = do
  getFiles <- myDoCall (lsFile $ Just dirName) h (Just "8000")
  case getFiles of
    Left err -> do
      putStrLn "error listing files..."
    Right ((FsContents dir files):_) -> do
      case files of
        [] -> do putStrLn "directory empty..."
        [x] -> do putStrLn $ "Files: " ++ x
        xs -> do putStrLn $ "Files: " ++ (DL.intercalate "\n " xs)

-- can combine this logic with download when integrating
doFileQuery :: String -> String -> Maybe String -> Maybe String -> IO ()
doFileQuery fileName dirName h p = do
  getRef <- myDoCall (fileQuery $ Message fileName dirName) h (Just "8000")
  case getRef of
    Left err -> do
      putStrLn "error querying file..."
    Right ((FileRef fPath fID "time" fsIP fsPort):_) -> do
      -- integrate download here!
      putStrLn ("ID: " ++ fID ++ "\nIP: " ++ fsIP ++ "\nPort: " ++ fsPort)


-- can combine this logic with upload when integrating
doMapFile :: String -> String -> Maybe String -> Maybe String -> IO ()
doMapFile fileName dirName h p = do
  getMapping <- myDoCall (mapFile $ Message fileName dirName) h (Just "8000")
  case getMapping of
    Left err -> do
      putStrLn "error mapping file..."
    Right ((FileRef fPath fID "time" fsIP fsPort):_) -> do
      -- integrate upload here!
      putStrLn ("ID: " ++ fID ++ "\nIP: " ++ fsIP ++ "\nPort: " ++ fsPort)

-- transaction service commands
doBeginTrans :: Maybe String -> Maybe String -> IO ()
doBeginTrans h p = do
  getTransID <- myDoCall beginTransaction h (Just "8001")
  let owner = "clientTransaction" :: String
  case getTransID of
    Left err -> do
      putStrLn "error obtaining transaction ID..."
    Right (ResponseData tID) -> do
      putStrLn ("New transaction ID: " ++ tID)
      -- store tID in client db
      withClientMongoDbConnection $ do
        findTrans <- find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
        let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
        case myTrans of
          ((CurrentTrans _ oldTID):_) -> liftIO $ do
            putStrLn "Aborting current transaction and starting new one..."
            -- tell transaction server to abort previous transaction
            doCall (abort oldTID) h (Just "8001")
            -- set transaction boolean to true ?? or just db entry existence as check...
            withClientMongoDbConnection $ upsert (select ["tOwner" =: owner] "MY_TID") $ toBSON (CurrentTrans owner tID)
          [] -> liftIO $ do
            putStrLn "starting new transaction..."
            -- set transaction boolean to true ?? or just db entry existence as check...
            withClientMongoDbConnection $ upsert (select ["tOwner" =: owner] "MY_TID") $ toBSON (CurrentTrans owner tID)


-- fetch tID from client db, talk to transaction server, remove ID from db
doCommit :: Maybe String -> Maybe String -> IO ()
doCommit h p = do
  let owner = "clientTransaction" :: String
  withClientMongoDbConnection $ do
    findTrans <- find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
    let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
    case myTrans of
      ((CurrentTrans _ tID):_) -> liftIO $ do
        putStrLn "Committing current transaction..."
        doCall (commit tID) h (Just "8001")
        withClientMongoDbConnection $ delete (select ["tOwner" =: owner] "MY_TID")
      [] -> liftIO $ do
        putStrLn "No active transactions..."

-- fetch tID from client db, talk to transaction server, remove ID from db
doAbort :: Maybe String -> Maybe String -> IO ()
doAbort h p = do
  let owner = "clientTransaction" :: String
  withClientMongoDbConnection $ do
    findTrans <- find (select ["tOwner" =: owner] "MY_TID") >>= drainCursor
    let myTrans = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CurrentTrans) findTrans
    case myTrans of
      ((CurrentTrans _ tID):_) -> liftIO $ do
        putStrLn "Aborting current transaction..."
        doCall (abort tID) h (Just "8001")
        withClientMongoDbConnection $ delete (select ["tOwner" =: owner] "MY_TID")
      [] -> liftIO $ do
        putStrLn "No active transactions..."

-- | The options handling

-- First we invoke the options on the entry point.
someFunc :: IO ()
someFunc = do
  banner
  join $ execParser =<< opts

-- | Defined in the applicative style, opts provides a declaration of the entire command line
--   parser structure. To add a new command just follow the example of the existing commands. A
--   new 'doCall' function should be defined for your new command line option, with a type matching the
--   ordering of the application of arguments in the <$> arg1 <*> arg2 .. <*> argN style below.
opts :: IO (ParserInfo (IO ()))
opts = do
  progName <- getProgName

  return $ info (   helper
                <*> subparser
                       (  command "load-envs"
                                  (withInfo ( doLoadEnvVars
                                            <$> optional (strOption ( long "name"
                                                                   <> short 'n'
                                                                   <> help "The variable to load."))
                                            <*> serverIpOption
                                            <*> serverPortOption) "Load an environment variable on the remote server." )
                       <> command "get-readme"
                                  (withInfo ( doGetREADME
                                          <$> serverIpOption
                                          <*> serverPortOption) "Get a remote README file." )
                       <> command "store-message"
                                  (withInfo ( doStoreMessage
                                          <$> argument str (metavar "Name")
                                          <*> argument str (metavar "Message")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Store a message on the remote server." )
                       <> command "search-message"
                                  (withInfo ( doSearchMessage
                                          <$> argument str (metavar "Name")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Search for messages on the remote server." )
                       <> command "rest-call"
                                   (withInfo ( doPerformRestCall
                                           <$> optional (strOption ( long "search"
                                                                  <> short 's'
                                                                  <> help "The search string for the hackage call."))
                                           <*> serverIpOption
                                           <*> serverPortOption) "Do a hackage rest call from the remote server." )
                       <> command "sign-up"
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
                      -- <> command "log-out"
                        --           (withInfo ( doLogOut
                          --                 <$> argument str (metavar "UserName")
                            --               <*> argument str (metavar "Password")
                              --             <*> serverIpOption
                                --           <*> serverPortOption) "Logs user out of the remote server." )
                       <> command "lock"
                                   (withInfo ( doLockFile
                                            <$> argument str (metavar "fName")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Lock a file." )
                       <> command "unlock"
                                   (withInfo ( doUnlockFile
                                            <$> argument str (metavar "fName")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Unlock a file." )
                       <> command "is-locked"
                                   (withInfo ( doFileLocked
                                            <$> argument str (metavar "fName")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Check if file is locked." )
                       <> command "download"
                                   (withInfo ( doDownloadFile
                                            <$> argument str (metavar "fPath")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Download a file." )
                       <> command "upload"
                                   (withInfo ( doUploadFile
                                            <$> argument str (metavar "fPath")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Upload a file." )
                       <> command "ls-dir"
                                   (withInfo ( doLsDir
                                            <$> serverIpOption
                                            <*> serverPortOption) "List directories in the file system." )
                       <> command "ls-file"
                                   (withInfo ( doLsFile
                                            <$> argument str (metavar "fDir")
                                            <*> serverIpOption
                                            <*> serverPortOption) "List files contained in a directory." )
                       <> command "file-query"
                                   (withInfo ( doFileQuery
                                            <$> argument str (metavar "fPath")
                                            <*> argument str (metavar "temp")
                                            <*> serverIpOption
                                            <*> serverPortOption) "Init download communication." )
                       <> command "map-file"
                                   (withInfo ( doMapFile
                                            <$> argument str (metavar "fPath")
                                            <*> argument str (metavar "temp")
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
             <> progDesc (progName ++ " is a simple test client for the use-haskell service." ++
                          " Try " ++ whiteCode ++ progName ++ " --help " ++ resetCode ++ " for more information. To " ++
                          " see the details of any command, " ++  "try " ++ whiteCode ++ progName ++ " COMMAND --help" ++
                          resetCode ++ ". The application supports bash completion. To enable, " ++
                          "ensure you have bash-completion installed and enabled (see your OS for details), the " ++
                          whiteCode ++ progName ++ resetCode ++
                          " application in your PATH, and place the following in your ~/.bash_profile : " ++ whiteCode ++
                          "source < (" ++ progName ++ " --bash-completion-script `which " ++ progName ++ "`)" ++
                          resetCode )
             <> header  (redCode ++ "Git revision : " ++ gitRev ++ ", branch: " ++ gitBranch ++ resetCode))

-- MongoDB client info
withClientMongoDbConnection :: Action IO a -> IO a
withClientMongoDbConnection act  = do
  let port = 27017
      database = "USEHASKELLDB"
  pipe <- connect (host "127.0.0.1")
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

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
-- This uses an applicative programming style that is very condensed, and easy to understand when you get used to it,
-- compared to the alternative sequence of calls and subsequent record construction.

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

   -- note that this is a good example of a commonly required function that could usefully be put in a shared library
   -- but I'm not going to do that for now.

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

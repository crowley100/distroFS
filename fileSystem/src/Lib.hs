{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}


module Lib
    ( startFunc
    ) where

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger

-- client -> server
data Login = Login { email :: String
                       , password :: String
                       } deriving (Generic, FromJSON, ToBSON, FromBSON)
                       
deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

-- server -> client
data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON)
                                 
type API = "load_environment_variables" :> QueryParam "email" String :> Get '[JSON] ResponseData
      :<|> "getREADME"                  :> Get '[JSON] ResponseData
      :<|> "storeLogin"                 :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
      :<|> "searchMessage"              :> QueryParam "email" String :> Get '[JSON] [ResponseData]
      :<|> "performRESTCall"            :> QueryParam "filter" String  :> Get '[JSON] ResponseData

startServer :: IO () -- set up wai logger for service to output apache style logging for rest calls
startServer = withLogging $ \ aplogger -> do

    warnLog "Starting fileSystem."
    
    let settings = setPort 8080  setLogger aplogger defaultSettings
    runSettings settings app
    
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = loadEnvironmentVariable
    :<|> getREADME
    :<|> storeLogin
    :<|> searchMessage
    :<|> performRESTCall
    
    where
        loadEnvironmentVariable :: Maybe String -> Handler ResponseData
        loadEnvironmentVariable ms = liftIO $ do
            warnLog $ "request to load environment variable: " ++ show ms
            case ms of
                Nothing -> do
                    warnLog "No environment variable requested"
                    return $ ResponseData "No environment variable requested... :O"
                Just s -> liftIO $ do
                    e <- lookupEnv s -- lookupEnv is an IO function, so we must use the `<-` notation
                    case e of
                      -- If the environment variable is not set, then create a lof entry and return and exception
                      Nothing -> do
                        warnLog $ "Environment variable " ++ s ++ " is not set."
                        return $ ResponseData $  "Environment variable " ++ s ++ " is not set."

                      -- Otherwise, return the envrionment variable. Note how variable names can use ', eg. x', x''
                      -- Haskell programmers often use this format for variables that are essentially referring to the same thing
                      Just e' -> return $ ResponseData e'
        
        getREADME :: Handler ResponseData
        getREADME = liftIO $ ResponseData <$> (readFile . head =<< getArgs)
        
        storeLogin :: Login -> Handler Bool
        storeLogin msg@(Login key _) = liftIO $ do
            warnLog $ "Storing message under key " ++ key ++ "."

            -- upsert creates a new record if the identified record does not exist, or if
            -- it does exist, it updates the record with the passed document content
            -- As you can see, this takes a single line of code
            withMongoDbConnection $ upsert (select ["email" =: key] "LOGIN_RECORD") $ toBSON msg

            return True  -- as this is a simple demo I'm not checking anything
          
        searchMessage :: Maybe String -> Handler [ResponseData]
        searchMessage (Just key) = liftIO $ do
            warnLog $ "Searching for value for key: " ++ key
            withMongoDbConnection $ do
                docs <- find (select ["email" =: key] "LOGIN_RECORD") >>= drainCursor -- >>= chains fns
                return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe ResponseData) docs
        
        searchMessage Nothing = liftIO $ do
            warnLog $ "No key for searching."
            return $ ([] :: [ResponseData])
            
        performRESTCall :: Maybe String -> Handler ResponseData
        performRESTCall (Just filt) = liftIO $ do
            warnLog $ "recieved request to perform REST call with param " ++ filt
            doRest $ DL.filter (DL.isInfixOf filt)
            
        performRESTCall Nothing = liftIO $ do
            warnLog $ "recieved request to perform REST call, but no param "
            doRest id
            
        doRest :: ([String] -> [String]) -> IO ResponseData
        doRest flt = do
            -- first we perform the call to hackage.org, then we will extract the package names and filter
            -- to include only package names matching the 'filt' parameter, returning a comma separated string of those
            -- packages.
            res <- SC.runClientM getPackages =<< env   -- the actual REST call
            case res of
                Left err -> do
                    warnLog $ "Rest call failed with error: " ++ show err
                    return $ ResponseData $ "Rest call failed with error: " ++ show err
                Right pkgs -> do
                    return $ ResponseData $ DL.intercalate ", " $                    -- reduce to comma separated string
                                      flt $                                          -- run the filtering function
                                      DL.map (unpack . RestClient.packageName) pkgs  -- extract name and convert to string
            where env = do
                 manager <- newManager defaultManagerSettings
                 return (SC.ClientEnv manager (SC.BaseUrl SC.Http "hackage.haskell.org" 80 ""))
                 
-- helper functions...
-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do
    lname  <- getProgName
    llevel <- logLevel
    updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
    act aplogger


-- | Mongodb helpers...
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
    ip <- mongoDbIp
    port <- mongoDbPort
    database <- mongoDbDatabase
    pipe <- connect (host ip)
    ret <- runResourceT $ liftIO $ access pipe master (pack database) act
    close pipe
    return ret

-- | helper method to ensure we force extraction of all results
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def

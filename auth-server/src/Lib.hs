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
--import           Crypto.Cipher
--import           Crypto.Cipher.Types
import           Crypto.Cipher.AES
import           Codec.Crypto.RSA
--import           Codec.Crypto.AES
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
import           System.Random
import           UseHaskellAPI
import           UseHaskellAPIServer

-- type signature correct?
--sharedSecret :: [Label] -> AES
--sharedSecret = do
--  let bseed = (BS.pack $ "our secret")
--  return $ initKey bseed
--sharedSeed = "ourSecret"
seshSeed = "testKey"

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting auth-server."
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  --let x = PublicKey 258
    --  keys = generateKeyPair r 496
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = loadEnvironmentVariable
    :<|> getREADME
    :<|> storeMessage
    :<|> logIn
    :<|> signUp
    :<|> searchMessage
    :<|> performRESTCall

  where
    loadEnvironmentVariable :: Maybe String -> Handler ResponseData
    loadEnvironmentVariable ms = liftIO $ do
      warnLog $ "request to load environment variable: " ++ show ms
      case ms of
        Nothing -> do
          warnLog "No environment variable requested"
          return $ ResponseData "WAT? No environment variable requested."
        Just s  -> liftIO $ do
          e <- lookupEnv s
          case e of
            -- If the environment variable is not set, then create a lof entry and return and exception
            Nothing -> do
              warnLog $ "Environment variable " ++ s ++ " is not set."
              return $ ResponseData $  "Environment variable " ++ s ++ " is not set."
            Just e' -> return $ ResponseData e'

    getREADME :: Handler ResponseData -- fns with no input
    getREADME = liftIO $ ResponseData <$> (readFile . head =<< getArgs)

    storeMessage :: Message -> Handler Bool
    storeMessage msg@(Message key _) = liftIO $ do
      warnLog $ "Storing message under key [" ++ key ++ "]."
      -- upsert creates a new record if the identified record does not exist, or if
      -- it does exist, it updates the record with the passed document content
      withMongoDbConnection $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg

      return True  -- as this is a demo, not checking anything

    signUp :: Login -> Handler ResponseData -- bool?
    signUp val@(Login key pass) = liftIO $ do
      warnLog $ "Checking if user in db: " ++ key
      withMongoDbConnection $ do
        docs <- findOne (select ["userName" =: key] "USER_RECORD")
        case docs of
          Nothing -> liftIO $ do
            -- decrypt pass with private key first HERE!
            hash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack pass)
            let pwd = BS.unpack $ fromJust hash
            withMongoDbConnection $ upsert (select ["userName" =: key] "USER_RECORD") $ toBSON val {password = pwd}
            return $ ResponseData $ "Success"
          Just _ -> return $ ResponseData $ "Account already exists."

    checkPass :: [Login] -> String -> Bool
    checkPass ((Login _ hash):_) pass = validatePassword (BS.pack hash) (BS.pack pass)
    checkpass _ _ = False

    -- appends null ('\0') characters until multiple of 16
    aesPad :: String -> String
    aesPad text
      | ((mod (length text) 16) == 0) = text
      | otherwise = aesPad (text ++ "\0")

    -- strips null ('\0') characters from end of string
    aesUnpad :: String -> String
    aesUnpad text = takeWhile (/= '\0') text

    -- seed -> string_to_encrypt -> encrypted_string
    myEncryptAES :: String -> String -> String
    myEncryptAES seed text = do
      let bseed = (BS.pack $ aesPad seed)
          btext = (BS.pack $ aesPad text)
      let myKey = initKey bseed
      let encryption = encryptECB myKey btext
      BS.unpack encryption

    -- seed -> string_to_decrypt -> decrypted_string (unpadded)
    myDecryptAES :: String -> String -> String
    myDecryptAES seed text = do
      let bseed = (BS.pack $ aesPad seed)
          btext = (BS.pack text)
      let myKey = initKey bseed
      let decryption = decryptECB myKey btext
      aesUnpad $ BS.unpack decryption

    -- return Token: (ss(Ticket),seshKey,expiryDate)
    logIn :: Login -> Handler [ResponseData]
    logIn val@(Login key pass) = liftIO $ do
      warnLog $ "Logging user in: " ++ key
      withMongoDbConnection $ do
        -- decrypt pass with private key first HERE
        -- create symmetric AES key from pass
        docs <- find (select ["userName" =: key] "USER_RECORD") >>= drainCursor
        let hashedPass = take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Login) docs
        let valid = checkPass hashedPass pass
        case valid of
          False -> return $ ((ResponseData $ "FAIL"):[])
          True -> do
            --let sesh_key = initKey $ BS.pack $ "testKey"
            let ss = sharedSeed
            let ticket = myEncryptAES (ss) (seshSeed)
                encSesh = myEncryptAES (pass) (seshSeed) -- using '|' as delimiter
            let ticket1 = myDecryptAES (ss) (ticket)
                encSesh1 = myDecryptAES (pass) (encSesh)
                -- other servers can extract date from ticket and cmp before servicing
            --let encrypion = crypt CBC $ $ "16" Encrypt $ pwd
            return $ ((ResponseData $ ticket1):(ResponseData $ encSesh1):[]) -- RETURN 'TOKEN' WITH HASHED CONTENTS

    searchMessage :: Maybe String -> Handler [Message]
    searchMessage (Just key) = liftIO $ do
      warnLog $ "Searching for value for key: " ++ key

      withMongoDbConnection $ do
        docs <- find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor
        --warnLog $ "retrieved data: " ++ show docs
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

    searchMessage Nothing = liftIO $ do
      warnLog $ "No key for searching."
      return $ ([] :: [Message])

    -- | Performing a REST call
    -- The following function performs a REST call to a remote service 'hackage.haskell.org'. This remote service is a
    -- searchable documentation server. The API to the service is accessible at http://hackage.haskell.org
    performRESTCall :: Maybe String -> Handler ResponseData
    performRESTCall (Just filt) = liftIO $ do
      warnLog $ "recieved request to perform REST call with param " ++ filt
      doRest $ DL.filter (DL.isInfixOf filt)

    -- | An implementation when no parameter is passed, no filtering so.
    performRESTCall Nothing = liftIO $ do
      warnLog $ "recieved request to perform REST call, but no param "
      doRest id

    -- | the performRESTCall is delegated to this function, with a filtering function passed as a parameter
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
          return $ ResponseData $ DL.intercalate ", " $                          -- reduce to comma separated string
                                  flt $                                          -- run the filtering function
                                  DL.map (unpack . RestClient.packageName) pkgs  -- extract name and convert to string
      where env = do
             manager <- newManager defaultManagerSettings
             return (SC.ClientEnv manager (SC.BaseUrl SC.Http "hackage.haskell.org" 80 ""))

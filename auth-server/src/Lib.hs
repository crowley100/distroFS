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
import           Crypto.Cipher.AES
import           Codec.Crypto.RSA
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as C
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
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
import           System.Random
import           UseHaskellAPI
import           UseHaskellAPIServer
import           RSAhelpers
import           Crypto.Random.DRBG

seshSeed = "testKey"

startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting auth-server."
  let settings = setPort authPort $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api authServer

api :: Proxy AuthAPI
api = Proxy

authServer :: Server AuthAPI
authServer = signUp
        :<|> logIn
        :<|> loadPublicKey
  where
    -- | Client's first contact with the system, registration with authentication service.
    signUp :: Login -> Handler ResponseData -- bool?
    signUp val@(Login key pass) = liftIO $ do
      warnLog $ "Checking if user in db: " ++ key
      passText <- decryptPass pass -- decrypt client pass with private key
      withMongoDbConnection $ do
        docs <- findOne (select ["userName" =: key] "USER_RECORD")
        case docs of
          Nothing -> liftIO $ do
            hash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack passText)
            let pwd = BS.unpack $ fromJust hash
            liftIO $ do
              warnLog $ "hash to be stored: " ++ pwd ++ " || of length: " ++ (show (length pwd))
            withMongoDbConnection $ upsert (select ["userName" =: key] "USER_RECORD") $ toBSON val {password = pwd}
            return $ ResponseData $ "Success"
          Just _ -> return $ ResponseData $ "Account already exists."

    -- | Clients must login to authenticate communication with the rest of the system.
    logIn :: Login -> Handler [ResponseData]
    logIn val@(Login key pass) = liftIO $ do
      passText <- decryptPass pass -- decrypt client pass with private key
      expiryDate <- getExpiryDate
      warnLog $ "<<< DECRYPTED PASS >>> :: [" ++ passText ++ "]" ++ " of length: " ++ (show (length passText))
      withMongoDbConnection $ do
        docs <- find (select ["userName" =: key] "USER_RECORD") >>= drainCursor
        let hashedPass@((Login _ x):_) = take 1 $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Login) docs
        let valid = checkPass hashedPass passText
        case valid of
          False -> return $ ((ResponseData $ "FAIL"):[])
          True -> do
            let ss = (aesPad sharedSeed)
            let ticket = myEncryptAES (ss) (seshSeed)
                encSesh = myEncryptAES (aesPad passText) (seshSeed) -- encrypt symmetric AES key using pass
                encDate = myEncryptAES (aesPad passText) (expiryDate)
            return $ ((ResponseData ticket):(ResponseData encSesh):(ResponseData encDate):[])

    -- | Handler for clients to fetch the authentication server's public key
    loadPublicKey :: Handler [ResponseData]
    loadPublicKey = liftIO $ do
      withMongoDbConnection $ do
        let auth = "auth" :: String
        docs <- find (select ["owner" =: auth] "Keys") >>= drainCursor
        let pubKey= catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Keys) docs
        case pubKey of
          [(Keys _ pub prv)]-> return $ toResponseData pub
          [] -> liftIO $ do
            r1 <- newGenIO :: IO HashDRBG
            let (pub,priv,g2) = generateKeyPair r1 1024
            let strPubKey = fromPublicKey pub
            let strPrvKey = fromPrivateKey priv
            let key = Keys auth strPubKey strPrvKey
            withMongoDbConnection $ upsert (select  ["owner" =: auth] "Keys") $ toBSON key
            return $ toResponseData strPubKey

-- helper functions
-- | Validate password against hash stored in db.
checkPass :: [Login] -> String -> Bool
checkPass ((Login _ hash):_) pass = validatePassword (BS.pack hash) (BS.pack pass)
checkpass _ _ = False

-- | Decrypt user's password using authentication service's private key.
decryptPass :: String ->  IO String
decryptPass password = do
    let auth = "auth" :: String
    withMongoDbConnection $ do
      keypair <- find (select ["owner" =: auth] "Keys") >>= drainCursor
      let [(Keys _ pub prv)]= catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Keys) keypair
      let prvKey = toPrivateKey prv -- returns PrivateKey data type
      let pass = C.pack password -- converts  into [word8] Codec.Binary.UTF8.String
      let dpass = decrypt prvKey pass
      return $ C.unpack dpass

-- | Returns a date 1 day from now.
getExpiryDate :: IO String
getExpiryDate = do
  let dayLength = posixDayLength
  currentTime <- getCurrentTime
  let lifespan = show $ addUTCTime dayLength currentTime
  return lifespan

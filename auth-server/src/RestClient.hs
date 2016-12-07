{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module RestClient where -- (getUsers, getUser, getPackages, Package(..))  where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client


-- The purpose of this section is to explain how to perform a REST call on a remote service fro your own Servant
-- service. This code will be called from the Handler doRESTCall in the handler set above.
--
-- We will access the REST serivice hackage.haskell.org, availabel on port 80. This service provides a set of endpoints
-- for haskell documentation. We will implemnt a single endpoint. For a more comprehensive example, see
-- https://haskell-servant.github.io/client-in-5-minutes.html.

-- First up, some data types we need to define the API call we want to maketype Username = Text

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Eq, Show)

instance FromJSON UserSummary where
  parseJSON (Object o) =                -- note that we are using an alternative method for defining FromJSON here.
    UserSummary <$> o .: "username"     -- we could have used template supportK instead.
                <*> o .: "userid"

  parseJSON _ = mzero

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Eq, Show, Generic, FromJSON)

data Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic, FromJSON)


-- Next, the hackage API definition - this is the remote service
-- This defines the functions we  want to be able to call. That is all there is to it. We can now call these funtions,
-- passing in the apporpriate parameters, and returning the appropriate data from hackage.haskell.org.

type HackageAPI = "users" :> Get '[JSON] [UserSummary]
             :<|> "user" :> Capture "username" Username :> Get '[JSON] UserDetailed
             :<|> "packages" :> Get '[JSON] [Package]

hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy

getUsers :: ClientM  [UserSummary]
getUser :: Username -> ClientM UserDetailed
getPackages :: ClientM  [Package]

getUsers :<|> getUser :<|> getPackages = client hackageAPI

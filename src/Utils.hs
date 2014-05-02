{-# language ScopedTypeVariables, OverloadedStrings #-}

module Utils where

import Control.Applicative
import Control.Exception
import Data.Map.Strict as Map
import Database.PostgreSQL.Simple
import Data.Maybe
import Data.Yaml
import System.IO (stderr, hPrint)

data User = User {
    name :: String,
    password :: String
  }
    deriving (Show, Eq, Ord)

data Permission = Permission {
    schemaPermission :: String,
    tablePermission :: String,
    viewPermission :: String,
    schemaPattern :: String,
    tablePattern :: String,
    viewPattern :: String
  }
    deriving Show

instance FromJSON Permission where
    parseJSON (Object m) = Permission <$>
        m .: "schemaPermission" <*>
        m .: "tablePermission" <*>
        m .: "viewPermission" <*>
        m .: "schemaPattern" <*>
        m .: "tablePattern" <*>
        m .: "viewPattern"
    parseJSON x = fail ("perm: " ++ show x)

instance FromJSON ConnectInfo where
    parseJSON (Object m) = ConnectInfo <$>
        m .: "host" <*>
        m .: "port" <*>
        m .: "user" <*>
        (fromMaybe "" <$> (m .:? "password")) <*>
        m .: "database"
    parseJSON x = fail ("not an object: " ++ show x)

-- catch any error, print out as stderr, continue instead of stopping.
tryExec :: IO a -> IO ()
tryExec action = do
    x :: (Either SomeException a) <- try action
    case x of
        Left e -> hPrint stderr e
        _ -> return ()

-- * config files
readConnectInfo :: IO ConnectInfo
readConnectInfo =
    either (error . show) id <$>
    decodeFileEither "./config/db.cfg"

-- | Reads all the permissions for all users from the config files.
readUsersConfig :: IO (Map User [Permission])
readUsersConfig = do
    privs :: Map String [Permission] <- either (error . show) id <$>
        decodeFileEither "./config/privileges.cfg"
    passwords :: Map String String <- either (error . show) id <$>
        decodeFileEither "./config/credentials.cfg"
    let lookupPassword :: String -> User
        lookupPassword name =
            maybe
                (error ("missing credentials for user " ++ name))
                (User name)
                (Map.lookup name passwords)
    return $ mapKeys lookupPassword privs

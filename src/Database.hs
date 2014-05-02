{-# language ScopedTypeVariables, QuasiQuotes, FlexibleInstances, InstanceSigs #-}

module Database where

import Database.PostgreSQL.Simple (Connection, fromOnly, query_, execute_)
import Control.Applicative
import Data.List (intercalate)
import Text.InterpolatedString.Perl6 (qc)
import Control.Monad (forM_, when, unless)

import Utils

data GrantType = SCHEMA | TABLE | VIEW deriving (Show, Eq)
type SchemaPattern = String
type Username = String
type DBObject = String -- TableName, SchemaName, ViewName
type PermissionType = String -- "SELECT", "ALL", "USAGE", specified in privileges.cfg file
type Query = String

-- constructing grant query for tables / schemas
grantQuery :: GrantType -> Username -> [DBObject] -> PermissionType -> Query
grantQuery grant username objects permission  -- for Views grantting need TO NOT have granttype.
    | null objects = ""
    | grant == VIEW = "GRANT "  ++ permission ++ " ON " ++  intercalate ", " objects ++ " TO " ++ username ++ ";"
    | otherwise = "GRANT "  ++ permission ++ " ON " ++ show grant ++ " " ++  intercalate ", " objects ++ " TO " ++ username ++ ";"

revokeQuery :: GrantType -> Username -> [DBObject] -> PermissionType -> Query
revokeQuery grant username objects permission -- for Views grantting need TO NOT have granttype.
    | null objects = ""
    | grant == VIEW = "REVOKE " ++ permission ++ " ON " ++  intercalate ", " objects ++ " FROM " ++ username ++ ";"
    | otherwise = "REVOKE " ++ permission ++ " ON " ++ show grant ++ " " ++  intercalate ", " objects ++ " FROM " ++ username ++ ";"

transferOwnerQuery :: GrantType -> Username -> DBObject -> Query
transferOwnerQuery grant unameNew object =
    "ALTER " ++ show grant ++ " " ++ object ++ " OWNER TO " ++ unameNew ++ ";"

doesUserExist :: Connection -> Username -> IO Bool
doesUserExist db uname = do
    users :: [Username] <-
        fmap fromOnly <$>
        query_ db [qc|
            SELECT usename
            FROM pg_user
            WHERE usename = '{uname}';
            |]
    return $ length users == 1
    -- probably need to catch the error when length users > 1, but it is impossible anyway?

createUserIfMissing :: Connection -> User -> IO ()
createUserIfMissing db (User uname upassword) = do
    exist <- doesUserExist db uname
    unless exist $
        tryExec $ execute_ db [qc|
            CREATE USER {uname}
            WITH PASSWORD '{upassword}';
            |]

updateUserPasswordIfExists :: Connection -> User -> IO ()
updateUserPasswordIfExists db (User uname upassword) = do
    exist <- doesUserExist db uname
    when exist $
        tryExec $ execute_ db [qc|
            ALTER USER {uname}
            WITH PASSWORD '{upassword}';
            |]

-- SchemaPattern might be '%' to specify ALL Schema
revokeAllUsersPermissionIfExists :: Connection -> SchemaPattern -> Username -> IO ()
revokeAllUsersPermissionIfExists db schemaPattern uname = do
    exist <- doesUserExist db uname
    when exist $ do
        putStrLn $ "Revoking all schema privilges for user: " ++ uname
        uschemas :: [DBObject] <-
            fmap fromOnly <$>
            query_ db [qc|
                SELECT schema_name
                FROM information_schema.schemata
                WHERE schema_name like '{schemaPattern}';
                |]
        unless (null uschemas) $
            tryExec $ execute_ db [qc|
                {revokeQuery SCHEMA uname uschemas "ALL"}
                |]

        putStrLn $ "Revoking all table privilges for user: " ++ uname
        utables :: [DBObject] <-
            fmap fromOnly <$>
            query_ db [qc|
                SELECT schemaname || '.' || '"' || tablename || '"'
                FROM pg_tables
                WHERE schemaname like '{schemaPattern}';
                |]
        unless (null utables) $
            tryExec $ execute_ db [qc|
                {revokeQuery TABLE uname utables "ALL"}
                |]

        putStrLn $ "Revoking all view privilges for user: " ++ uname
        uviews :: [DBObject] <-
            fmap fromOnly <$>
            query_ db [qc|
                SELECT schemaname || '.' || '"' || viewname || '"'
                FROM pg_views
                WHERE schemaname like '{schemaPattern}';
                |]
        unless (null uviews) $
            tryExec $ execute_ db [qc|
                {revokeQuery VIEW uname uviews "ALL"}
                |] 

grantUserPermissionIfExists :: Connection -> SchemaPattern -> User -> Permission -> IO ()
grantUserPermissionIfExists db schemaPattern (User uname _) 
    (Permission uschemaPermission utablePermission uviewPermission uschemaPattern utablePattern uviewPattern) = do
        exist <- doesUserExist db uname
        when exist $ do
            putStrLn $ "Granting schema privilges for user: " ++ uname ++ " " ++ uschemaPermission ++ " " ++ uschemaPattern
            uschemas :: [DBObject] <-
                fmap fromOnly <$>
                query_ db [qc|
                    SELECT schema_name
                    FROM information_schema.schemata
                    WHERE schema_name like '{uschemaPattern}'
                    AND schema_name like '{schemaPattern}';
                    |]
            unless (null uschemas) $
                tryExec $ execute_ db [qc|
                    {grantQuery SCHEMA uname uschemas uschemaPermission}
                    |]

            putStrLn $ "Granting table privilges for user: " ++ uname ++ " " ++ utablePermission ++ " " ++ utablePattern
            utables :: [DBObject] <-
                fmap fromOnly <$>
                query_ db [qc|
                    SELECT schemaname || '.' || '"' || tablename || '"'
                    FROM pg_tables
                    WHERE schemaname like '{uschemaPattern}'
                    AND tablename like '{utablePattern}'
                    AND schemaname like '{schemaPattern}';
                    |]
            unless (null utables) $
                tryExec $ execute_ db [qc|
                    {grantQuery TABLE uname utables utablePermission}
                    |]

            putStrLn $ "Granting view privilges for user: " ++ uname ++ " " ++ uviewPermission ++ " " ++ uviewPattern
            uviews :: [DBObject] <-
                fmap fromOnly <$>
                query_ db [qc|
                    SELECT schemaname || '.' || '"' || viewname || '"'
                    FROM pg_views
                    WHERE schemaname like '{uschemaPattern}'
                    AND viewname like '{uviewPattern}'
                    AND schemaname like '{schemaPattern}';
                    |]
            unless (null uviews) $
                tryExec $ execute_ db [qc|
                    {grantQuery VIEW uname uviews uviewPermission}
                    |]

dropUserIfExists :: Connection -> Username -> IO ()
dropUserIfExists db uname = do
    exist <- doesUserExist db uname
    when exist $
        tryExec $ execute_ db [qc|
            DROP USER {uname};
            |]

transferAllOwnershipIfExists :: Connection -> Username -> Username -> IO ()
transferAllOwnershipIfExists db unameOld unameNew = do
    existOld <- doesUserExist db unameOld
    existNew <- doesUserExist db unameNew
    when (existOld && existNew) $ do
        putStrLn $ "Changing all schema ownerships from user: " ++ unameOld ++ " to user: " ++ unameNew
        allschemas :: [DBObject] <-
            fmap fromOnly <$>
            query_ db [qc|
                SELECT schema_name
                FROM information_schema.schemata
                WHERE schema_owner = '{unameOld}';
                |]
        forM_ allschemas $ \schema -> do
            putStrLn schema
            tryExec $ execute_ db [qc|
                {transferOwnerQuery SCHEMA unameNew schema}
                |]

        putStrLn $ "Changing all table ownerships from user: " ++ unameOld ++ " to user: " ++ unameNew
        alltables :: [DBObject] <-
            fmap fromOnly <$>
            query_ db [qc|
                SELECT schemaname || '.' || '"' || tablename || '"'
                FROM pg_tables
                WHERE tableowner = '{unameOld}';
                |]
        forM_ alltables $ \table -> do
            putStrLn table
            tryExec $ execute_ db [qc|
                {transferOwnerQuery TABLE unameNew table}
                |]

        putStrLn $ "Changing all view ownerships from user: " ++ unameOld ++ " to user: " ++ unameNew
        allviews :: [DBObject] <-
            fmap fromOnly <$>
            query_ db [qc|
                SELECT schemaname || '.' || '"' || viewname || '"'
                FROM pg_views
                WHERE viewowner = '{unameOld}';
                |]
        forM_ allviews $ \view -> do
            putStrLn view
            tryExec $ execute_ db [qc|
                {transferOwnerQuery VIEW unameNew view}
                |]

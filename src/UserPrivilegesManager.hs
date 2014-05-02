import Control.Exception
import Database.PostgreSQL.Simple (ConnectInfo, close, connect)
import Data.Map (toList)
import Data.Char (toUpper)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs (cmdArgsRun)

import qualified Utils as UT
import qualified Database as DB
import qualified CmdArguments as CA

-- aux
updateUserAccount :: ConnectInfo -> [(UT.User,[UT.Permission])] -> IO ()
updateUserAccount connectInfo usersConfig = do
    putStrLn "Updating all user accounts..."
    bracket (connect connectInfo) close $ \db  ->
        forM_ usersConfig $ \(user,permissions) -> do
            DB.createUserIfMissing db user
            DB.updateUserPasswordIfExists db user

revokeAllUserPermission :: ConnectInfo -> DB.SchemaPattern -> [(UT.User,[UT.Permission])] -> IO ()
revokeAllUserPermission connectInfo schemaPattern usersConfig = do
    putStrLn "Revoking all users permissions..."
    bracket (connect connectInfo) close $ \db  ->
        forM_ (map (UT.name.fst) usersConfig) $ \uname ->
        DB.revokeAllUsersPermissionIfExists db schemaPattern uname

updateUserPermission :: ConnectInfo -> DB.SchemaPattern -> [(UT.User,[UT.Permission])] -> IO ()
updateUserPermission connectInfo schemaPattern usersConfig = do
    putStrLn "Updating all user permissions..."
    bracket (connect connectInfo) close $ \db  ->
        -- working on each permission
        forM_ usersConfig $ \(user,permissions) ->
            forM_ permissions $ \permission ->
                DB.grantUserPermissionIfExists db schemaPattern user permission

transferOwnership :: ConnectInfo -> DB.Username -> DB.Username -> IO ()
transferOwnership connectInfo unameOld unameNew  = do
    putStrLn $ "Transfering all ownership of " ++ unameOld ++ " to " ++ unameNew ++ "..."
    bracket (connect connectInfo) close $ \db ->
        DB.transferAllOwnershipIfExists db unameOld unameNew

dropUser :: ConnectInfo -> DB.Username -> IO ()
dropUser connectInfo uname = do
    putStrLn $ "Revoking all privileges and dropping " ++ uname ++ "..."
    bracket (connect connectInfo) close $ \db -> do
        DB.revokeAllUsersPermissionIfExists db "%" uname
        DB.dropUserIfExists db uname

-- main's methods
doUpdateAll :: ConnectInfo -> [(UT.User,[UT.Permission])] -> DB.SchemaPattern -> IO ()
doUpdateAll connectInfo usersConfig schemaPattern = do
    updateUserAccount connectInfo usersConfig
    updateUserPermission connectInfo schemaPattern usersConfig

-- doFullUpdateAll means revoke all permission and then update, cost more time but safer.
doFullUpdateAll :: ConnectInfo -> [(UT.User,[UT.Permission])] -> DB.SchemaPattern -> IO ()
doFullUpdateAll connectInfo usersConfig schemaPattern = do
    revokeAllUserPermission connectInfo schemaPattern usersConfig
    doUpdateAll connectInfo usersConfig schemaPattern

doTransferOwnership :: ConnectInfo -> DB.Username -> DB.Username -> Bool -> IO ()
doTransferOwnership connectInfo unameOld unameNew force
    | unameOld `elem` ["rdsdb"] = putStrLn "Transfering this user is prohibited."
    | force = transferOwnership connectInfo unameOld unameNew
    | otherwise = do
        putStrLn $ 
            "About to transfer all ownerships of " ++ 
            unameOld ++ " to " ++ unameNew ++ ". Are you sure? (Y/N)"
        confirm <- getChar
        when (toUpper confirm == 'Y') $ transferOwnership connectInfo unameOld unameNew

doDropUser :: ConnectInfo -> DB.Username -> Bool -> IO ()
doDropUser connectInfo uname force
    | uname `elem` ["rdsdb"] = putStrLn "Deleting this user is prohibited."
    | force = dropUser connectInfo uname
    | otherwise = do
        putStrLn $ 
            "About to remove " ++ uname ++ ". Are you sure? (Y/N)"
        confirm <- getChar
        when (toUpper confirm == 'Y') $ dropUser connectInfo uname

-- main
main :: IO ()
main = do
    opts <- cmdArgsRun CA.myModes
    connectInfo <- UT.readConnectInfo
    usersConfig <- UT.readUsersConfig    
    case opts of 
        CA.UpdateMode schemaName ->
            doUpdateAll connectInfo (toList usersConfig) (fromMaybe "%" schemaName)
        CA.FullUpdateMode schemaName ->
            doFullUpdateAll connectInfo (toList usersConfig) (fromMaybe "%" schemaName)
        CA.TransferMode unameOld unameNew force ->
            doTransferOwnership connectInfo unameOld unameNew force
        CA.DropMode uname force ->
            doDropUser connectInfo uname force

{-# language DeriveDataTypeable #-}

module CmdArguments where

import System.Console.CmdArgs

-- program info 
_PROGRAM_NAME = "PostgreSQL User Privileges Manager"
_PROGRAM_VERSION = "0.1.0.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A Haskell tool for managing user privileges in PostgreSQL."
_COPYRIGHT = "(C) Dat Le 2013"

-- handle argument options:
data MyOptions = UpdateMode{schemaName :: Maybe String}
               | FullUpdateMode{schemaName :: Maybe String}
               | TransferMode{oldUname :: String, newUname :: String, force :: Bool}
               | DropMode{uname :: String, force :: Bool}
               deriving (Data, Typeable, Show, Eq)

updateMode :: MyOptions
updateMode = UpdateMode
    { schemaName = Nothing &= help "SCHEMA_PATTERN"
    }
    &= details  [ "Examples:"
                , "    .cabal-sandbox/bin/user-privileges-manager updatemode -s schema_pattern"
                ]

fullUpdateMode :: MyOptions
fullUpdateMode = FullUpdateMode
    { schemaName = Nothing &= help "SCHEMA_PATTERN"
    }
    &= details  [ "Examples:"
                , "    .cabal-sandbox/bin/user-privileges-manager fullupdatemode -s schema_pattern"
                ]

transferMode :: MyOptions
transferMode = TransferMode
    { oldUname = def &= typ "OLD" &= argPos 0 --positional args, not option
    , newUname = def &= typ "NEW" &= argPos 1
    , force = False &= help "force transfer without prompting confirmation."
    }
    &= details  [ "Examples:"
                , "    .cabal-sandbox/bin/user-privileges-manager transfermode user1 user2 -f"
                ]

dropMode :: MyOptions
dropMode = DropMode
    { uname = def &= typ "USERNAME" &= argPos 0 --positional args, not option
    , force = False &= help "force drop without prompting confirmation."
    }
    &= details  [ "Examples:"
                , "    runhaskell UserPrivilegesManager.hs dropmode user1 -f"
                ]
 
myModes :: Mode (CmdArgs MyOptions)
myModes = cmdArgsMode $ modes [updateMode, transferMode, fullUpdateMode, dropMode]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME


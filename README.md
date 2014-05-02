postgresql-user-manager
=======================

To *functionally* manage User Privileges for PostgreSQL Database.

Allow administrator to easily:

    + Manage all user privileges specified in ./config/privileges.cfg file.
    + Transfer all ownership of one User to another.
    + Revoke all privileges of any user and / or Drop the user himself.

## Prerequisites

    + Ubuntu (preferably 12.04)

    + GHC 7.6.3 or later

    + Cabal 1.19.2 or later

    + Happy 1.19 or later

    + Credentials files (Located in config/Credentials/):
        - credentials.cfg: all user credentials that need to be managed
        - db.cfg: PostgreSQL's credentials used by psql commandline tool

## Executables

    To build the project (Go grab some coffee if it is your first time):
    $ ./runBuild.sh <project_home_folder> <cabal_build arguments>

    Show help:
    $ .cabal-sandbox/bin/user-privileges-manager --help

    To FULLY refresh all user privileges: (including remove all current privileges 
    and update with the new privileges)
    $ .cabal-sandbox/bin/user-privileges-manager fullupdatemode

    To refresh all user privileges:
    $ .cabal-sandbox/bin/user-privileges-manager updatemode

    To refresh all user privileges on some schemas:
    $ .cabal-sandbox/bin/user-privileges-manager updatemode -s %pattern%

    To transfer all ownership between 2 users:
    $ .cabal-sandbox/bin/user-privileges-manager transfermode user1 user2 -f

    To drop a user:
    $ .cabal-sandbox/bin/user-privileges-manager dropmode user1 -f

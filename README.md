postgresql-user-manager
=======================

A command-line interface tool to *functionally* manage User Privileges for Amazon Redshift or any PostgreSQL Database.

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

## Clone and build:

    Clone the repo:
    $ git clone https://github.com/zalora/postgresql-user-manager.git

    Build with Docker (remember to configure your credentials first in ./config folder)
    $ cd postgresql-user-manager/
    $ sudo docker build -t="postgresql-user-privileges" .

## Executables

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

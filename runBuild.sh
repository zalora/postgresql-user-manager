#!/bin/bash

set -o errexit

if [[ -z "$1" ]]
    then
    echo "Project Home is not specified, running with homeProject as current folder: $(pwd)"
    homeProject=$(pwd)
else
    echo "Project Home is specified as $1"
    homeProject=$1
fi
cd $homeProject
$HOME/.cabal/bin/cabal sandbox init
$HOME/.cabal/bin/cabal install $2 # $2 can be -j1 or --ghc-option=-fwarn-unused-imports ...
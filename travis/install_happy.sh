#!/usr/bin/sh

DIRECTORY=${HOME}
FILE=happys
DPATH=${DIRECTORY}/.cabal/bin/
FPATH="${DPATH}${FILE}"
if [ -e "$FPATH" ];
then
    echo "Happy exists. Do nothing"
else
    cabal install happy
fi

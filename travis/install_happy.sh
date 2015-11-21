#!/usr/bin/sh

DIRECTORY=${HOME}
FILE=happy
PATH=${DIRECTORY}/.cabal/bin/
FPATH="${PATH}${FILE}"
if [ -e "$FPATH" ];
then
    echo "Happy exists. Do nothing"
else
    cabal install $FILE
fi

#!/bin/sh

if [ ! -e gds.sqlite3 ]; then
        sqlite3 gds.sqlite3 < schema.sql
fi
ARGS=`seq 1 100000`
ghc managetestgroup.hs &&
        time ./managetestgroup creategroup barney $ARGS

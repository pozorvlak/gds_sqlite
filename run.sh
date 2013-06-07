#!/bin/sh

if [ ! -e gds.sqlite3 ]; then
        sqlite3 gds.sqlite3 < schema.sql
fi
ARGS=`perl -E 'say $_ for 0..10000'`
ghc -rtsopts managetestgroup.hs &&
        time ./managetestgroup +RTS -h -RTS creategroup -g barney $ARGS

#!/bin/sh

ARGS=`perl -E 'say $_ for 0..10000'`
ghc -rtsopts managetestgroup.hs &&
        time ./managetestgroup +RTS -h -RTS creategroup -g barney $ARGS

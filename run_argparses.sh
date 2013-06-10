#!/bin/sh

ARGS=$(seq 1 10000)
ghc argparse_getargs.hs || exit 2
ghc -rtsopts -prof -fprof-auto argparse_cmdargs.hs || exit 3
echo "Testing System.Environment.getArgs with 10,000 arguments..."
time ./argparse_getargs $ARGS
echo "Testing System.Console.CmdArgs.cmdArgs with 10,000 arguments..."
time ./argparse_cmdargs +RTS -p -RTS $ARGS

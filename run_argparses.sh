#!/bin/sh

ARGS=$(seq 1 10000)
ghc argparse_getargs.hs
ghc argparse_cmdargs.hs
echo "Testing System.Environment.getArgs with 10,000 arguments..."
time ./argparse_getargs $ARGS
echo "Testing System.Console.CmdArgs.cmdArgs with 10,000 arguments..."
time ./argparse_cmdargs $ARGS

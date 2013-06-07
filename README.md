My friend @totherme had a Haskell program that inserted some stuff into a
SQLite database. Unfortunately, it was very very slow. I thought I'd try to
find out why it was slow.

It appears that the problem is in the System.Console.CmdArgs library; I've
tried to make a minimal program demonstrating the issue in
`argparse_cmdargs.hs`. Run `./run_argparses.sh` to compile and invoke this
program, and also a comparison program that uses System.Environment.getArgs.

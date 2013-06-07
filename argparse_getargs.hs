import System.Environment(getArgs)

main = do
  args <- getArgs
  print $ length args

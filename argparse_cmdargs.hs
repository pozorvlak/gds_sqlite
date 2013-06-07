{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs

data ManageTestGroup = CreateGroup
                       {  files :: [String] } deriving (Data, Typeable, Show)

createDefaults :: ManageTestGroup
createDefaults = CreateGroup
             { files = [] &= args }

printArgLength :: ManageTestGroup -> IO ()
printArgLength (CreateGroup filenames) = do
  print $ length filenames

main = do
  opts <- cmdArgs (modes [createDefaults])
  printArgLength opts

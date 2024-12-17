module Main where

import Control.Monad.Except
import Core.Parse
import System.Directory
import Test.HUnit

main :: IO ()
main = do
  let exampleDir = "../examples/"
  filenames <- listDirectory exampleDir
  let filepaths = map (exampleDir ++) filenames
  runTestTTAndExit (TestList $ map testFile filepaths)

testFile :: FilePath -> Test
testFile fp = TestCase $ do
  res <- runExceptT (parseFile fp)
  let name = "parse the file " ++ show fp
  assertEqual name (Right ()) (void res)
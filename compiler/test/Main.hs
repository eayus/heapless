module Main where

import Control.Monad.Except
import Core.Check
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
  res <- runExceptT $ do
    core <- parseFile fp
    _ <- typecheck core
    pure ()
  let name = "parse the file " ++ show fp
  assertEqual name (Right ()) (void res)

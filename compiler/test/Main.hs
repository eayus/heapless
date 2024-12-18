module Main where

import Control.Monad.Except
import Core.Check
import Core.Parse
import Data.Bifunctor
import System.Directory
import Test.HUnit

main :: IO ()
main = do
  let correctExampleDir = "../examples/correct/"
  correctFilenames <- listDirectory correctExampleDir
  let correctFilepaths = map (correctExampleDir ++) correctFilenames
  let correctTests = TestList $ map testCorrectFile correctFilepaths

  let incorrectExampleDir = "../examples/incorrect/"
  incorrectFilenames <- listDirectory incorrectExampleDir
  let incorrectFilepaths = map (incorrectExampleDir ++) incorrectFilenames
  let incorrectTests = TestList $ map testIncorrectFile incorrectFilepaths

  runTestTTAndExit (TestList [correctTests, incorrectTests])

testCorrectFile :: FilePath -> Test
testCorrectFile fp = TestCase $ do
  res <- runExceptT $ do
    core <- parseFile fp
    _ <- typecheck core
    pure ()
  let name = "process file " ++ show fp
  assertEqual name (Right ()) (void res)

testIncorrectFile :: FilePath -> Test
testIncorrectFile fp = TestCase $ do
  res <- runExceptT $ do
    core <- parseFile fp
    _ <- typecheck core
    pure ()
  let name = "process incorrect file " ++ show fp
  assertEqual name (Left ()) (bimap (const ()) (const ()) res)
module Main where

import Compile
import Control.Monad.Except
import Core.Check
import Core.Parse
import Data.List
import System.Directory
import Test.HUnit

main :: IO ()
main = do
  let correctExampleDir = "../examples/correct/"
  correctFilenames <- filter (".core" `isSuffixOf`) <$> listDirectory correctExampleDir
  let correctFilepaths = map (correctExampleDir ++) correctFilenames
  let correctTests = TestList $ map testCorrectFile correctFilepaths

  let incorrectExampleDir = "../examples/incorrect/"
  incorrectFilenames <- filter (".core" `isSuffixOf`) <$> listDirectory incorrectExampleDir
  let incorrectFilepaths = map (incorrectExampleDir ++) incorrectFilenames
  let incorrectTests = TestList $ map testIncorrectFile incorrectFilepaths

  runTestTTAndExit (TestList [correctTests, incorrectTests])

testCorrectFile :: FilePath -> Test
testCorrectFile fp = TestCase $ do
  runExceptT (compileCore False fp) >>= \case
    Left err -> assertFailure $ "While typechecking " ++ show fp ++ "\n" ++ err
    Right _ -> pure ()

testIncorrectFile :: FilePath -> Test
testIncorrectFile fp = TestCase $ do
  runExceptT (parseFile fp) >>= \case
    Left err -> assertFailure $ "Parsing " ++ show fp ++ " failed:\n" ++ err
    Right expr -> do
      runExceptT (typecheck expr) >>= \case
        Left _ -> pure ()
        Right _ -> assertFailure $ "Expected example " ++ show fp ++ " to not typecheck, but it actually succeeded"

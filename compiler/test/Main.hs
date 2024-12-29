module Main where

import Control.Monad.Except
import Core.Check
import Core.Norm.Reify
import Core.Parse
import Core.Uncurry
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
    typecheck core
  case res of
    Left err -> assertFailure err
    Right core -> do
      let pe = partialEval core
      let uc = ucNf pe
      putStrLn $ "\t" ++ show uc

testIncorrectFile :: FilePath -> Test
testIncorrectFile fp = TestCase $ do
  -- TODO: Assert that parsing succeeds, but typechecking fails?
  res <- runExceptT $ do
    core <- parseFile fp
    typecheck core
  case res of
    Left _ -> pure ()
    Right _ -> assertFailure $ "Expected example " ++ show fp ++ " to fail, but it actually succeeded"
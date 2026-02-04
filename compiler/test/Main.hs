module Main where

import Control.Monad.Except
import Core.Check
import Core.Norm.Reify
import Core.Parse
import Core.Uncurry
import System.Directory
import Test.HUnit
import UC.LambdaLift
import UC.Name

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
      let uc' = nameMain uc
      let ll = llMain uc'
      -- Writing the value to a file is an easy way to force its evaluation.
      -- This is necessary because evaluation could technically use 'undefined',
      -- and we want the test suite to check for that.
      writeFile "/dev/null" $ show ll

testIncorrectFile :: FilePath -> Test
testIncorrectFile fp = TestCase $ do
  runExceptT (parseFile fp) >>= \case
    Left err -> assertFailure $ "Parsing " ++ show fp ++ " failed:\n" ++ err
    Right expr -> do
      runExceptT (typecheck expr) >>= \case
        Left _ -> pure ()
        Right _ -> assertFailure $ "Expected example " ++ show fp ++ " to not typecheck, but it actually succeeded"

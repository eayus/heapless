module Main where

import Control.Monad.Except
import Core.Check
import Core.Norm.Reify
import Core.Parse
import System.Environment

main :: IO ()
main =
  runExceptT go >>= \case
    Left err -> putStrLn err
    Right () -> pure ()

go :: ExceptT String IO ()
go = do
  [filepath] <- lift getArgs
  expr <- parseFile filepath
  core <- typecheck expr
  let core' = partialEval core
  lift $ print core'
  lift $ putStrLn "OK :)"
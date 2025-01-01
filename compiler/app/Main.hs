module Main where

import Control.Monad.Except
import Core.Check
import Core.Norm.Reify
import Core.Parse
import Core.Uncurry
import LL.Codegen
import System.Environment
import UC.LambdaLift
import UC.Name

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
  lift $ putStrLn "\ncore"
  lift $ print core

  let core' = partialEval core
  lift $ putStrLn "\ncore'"
  lift $ print core'

  let uc = ucNf core'
  lift $ putStrLn "\nuc"
  lift $ print uc

  let uc' = nameMain uc
  lift $ putStrLn "\nuc'"
  lift $ print uc'

  let ll = llMain uc'
  lift $ putStrLn "\nll"
  lift $ print ll

  let rust = cgProg ll
  lift $ putStrLn rust
  lift $ putStrLn "\n\nOK :)"
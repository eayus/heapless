module Main where

import Control.Monad.Except
import Core.Check
import Core.Norm.Reify
import Core.Parse
import Core.Uncurry
import LL.Codegen
import System.Environment
import System.Process
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

  rust <- lift $ cgProg ll
  lift $ putStrLn rust
  -- lift $ putStrLn "\n\nOK :)"

  lift $ do
    system "mkdir -p .build"
    writeFile ".build/main.rs" rust
    system "rustc .build/main.rs --crate-type=staticlib -Cpanic=abort -O -o .build/lib.a"
    system "clang data/runtime.c -c -o .build/runtime.o"
    system "clang .build/runtime.o .build/lib.a -o .build/main"

  pure ()
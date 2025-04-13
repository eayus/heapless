module Main where

import Control.Monad.Except
import Core.Check
import Core.Norm.Reify
import Core.Parse
import Core.Uncurry
import LL.Codegen
import Surface.Parse qualified as S
import Surface.Check qualified as S
import System.Environment
import System.Process
import UC.LambdaLift
import UC.Name

main :: IO ()
main =
  runExceptT surface >>= \case
    Left err -> putStrLn err
    Right () -> pure ()

-- Process surface lanugage
surface :: ExceptT String IO ()
surface = do
  [filepath] <- lift getArgs
  prog <- S.parseFile filepath
  S.typecheck prog
  lift $ putStrLn "OK!"
  -- lift $ print prog

-- Process core lanugage
go :: ExceptT String IO ()
go = do
  [filepath] <- lift getArgs
  expr <- parseFile filepath

  core <- typecheck expr
  -- lift $ putStrLn "\ncore"
  -- lift $ print core

  let core' = partialEval core
  -- lift $ putStrLn "\ncore'"
  -- lift $ pPrint core'

  let uc = ucNf core'
  -- lift $ putStrLn "\nuc"
  -- lift $ pPrint uc

  let uc' = nameMain uc
  -- lift $ putStrLn "\nuc'"
  -- lift $ pPrint uc'

  let ll = llMain uc'
  -- lift $ putStrLn "\nll"
  -- lift $ pPrint ll

  rust <- lift $ cgProg ll
  -- lift $ putStrLn rust
  -- lift $ putStrLn "\n\nOK :)"

  lift $ do
    system "mkdir -p .build"
    writeFile ".build/main.rs" rust
    system "rustc .build/main.rs --crate-type=staticlib -Cpanic=abort -O -o .build/lib.a"
    system "clang data/runtime.c -c -o .build/runtime.o"
    system "clang .build/runtime.o .build/lib.a -o .build/main"

  pure ()
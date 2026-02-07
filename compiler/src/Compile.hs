module Compile (Compile, compileCore) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class
import Core.Check
import Core.Norm.Reify
import Core.Parse
import Core.Uncurry
import LL.Codegen
import System.Exit
import System.Process
import Text.Pretty.Simple
import UC.LambdaLift
import UC.Name

type Compile = ExceptT String IO

compileCore :: Bool -> FilePath -> Compile ()
compileCore dbg filepath = do
  expr <- parseFile filepath

  core <- typecheck expr
  when dbg do
    lift $ putStrLn "\ncore"
    lift $ print core

  let core' = partialEval core
  when dbg do
    lift $ putStrLn "\ncore'"
    lift $ pPrint core'

  let uc = ucNf core'
  when dbg do
    lift $ putStrLn "\nuc"
    lift $ pPrint uc

  let uc' = nameMain uc
  when dbg do
    lift $ putStrLn "\nuc'"
    lift $ pPrint uc'

  let ll = llMain uc'
  when dbg do
    lift $ putStrLn "\nll"
    lift $ pPrint ll

  rust <- lift $ cgProg ll
  when dbg do
    lift $ putStrLn rust
    lift $ putStrLn "\n\nOK :)"

  cmd "mkdir -p .build"
  lift $ writeFile ".build/main.rs" rust
  cmd "rustc .build/main.rs --crate-type=staticlib -Cpanic=abort -O -o .build/lib.a"
  cmd "clang data/runtime.c -c -o .build/runtime.o"
  cmd "clang .build/runtime.o .build/lib.a -o .build/main"

  pure ()

cmd :: String -> Compile ()
cmd s = lift (system s) >>= \case
  ExitSuccess -> pure ()
  ExitFailure _ -> throwError $ "Executing " ++ show s ++ " failed"

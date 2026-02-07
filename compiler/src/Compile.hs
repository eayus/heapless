module Compile (Compile, compileCore) where

import Control.Monad.Except
import Control.Monad.Trans.Class
import Core.Check
import Core.Norm.Reify
import Core.Parse
import Core.Uncurry
import LL.Codegen
import System.Exit
import System.Process
import UC.LambdaLift
import UC.Name

type Compile = ExceptT String IO

compileCore :: FilePath -> Compile ()
compileCore filepath = do
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

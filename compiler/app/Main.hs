module Main where

import Compile
import Control.Monad.Except
import Control.Monad.Trans.Class
import System.Environment

main :: IO ()
main =
  runExceptT (parseArgs >>= compileCore True) >>= \case
    Left err -> putStrLn err
    Right () -> putStrLn "OK!"

parseArgs :: Compile FilePath
parseArgs = lift getArgs >>= \case
  [filepath] -> pure filepath
  _ -> throwError "Expected exactly one argument (filepath to core file)"

{-
-- Process surface lanugage
processSurface :: ExceptT String IO ()
processSurface = do
  [filepath] <- lift getArgs
  S.typecheckFile filepath
  lift $ putStrLn "OK!"
-}

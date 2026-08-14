module Main where

import Fixen.LSP.Server (runFixenLanguageServer)
import System.Exit (ExitCode (..), exitWith)

main :: IO ()
main = do
  result <- runFixenLanguageServer
  exitWith $
    if result == 0
      then ExitSuccess
      else ExitFailure result

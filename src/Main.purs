module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import State

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

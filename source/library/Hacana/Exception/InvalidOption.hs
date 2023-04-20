module Hacana.Exception.InvalidOption where

import qualified Control.Monad.Catch as Catch

newtype InvalidOption
  = InvalidOption String
  deriving (Eq, Show)

instance Catch.Exception InvalidOption

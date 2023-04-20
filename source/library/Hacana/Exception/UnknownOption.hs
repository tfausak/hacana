module Hacana.Exception.UnknownOption where

import qualified Control.Monad.Catch as Catch

newtype UnknownOption
  = UnknownOption String
  deriving (Eq, Show)

instance Catch.Exception UnknownOption

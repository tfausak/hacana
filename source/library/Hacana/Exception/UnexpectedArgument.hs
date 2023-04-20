module Hacana.Exception.UnexpectedArgument where

import qualified Control.Monad.Catch as Catch

newtype UnexpectedArgument
  = UnexpectedArgument String
  deriving (Eq, Show)

instance Catch.Exception UnexpectedArgument

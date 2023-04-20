module Hacana.Extra.Either where

import qualified Control.Monad.Catch as Catch

fail :: MonadFail m => Either String a -> m a
fail = either Prelude.fail pure

throw :: (Catch.Exception e, Catch.MonadThrow m) => Either e a -> m a
throw = either Catch.throwM pure

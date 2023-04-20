module Hacana.Type.Config where

import qualified Hacana.Type.Flag as Flag
import qualified Control.Monad.Catch as Catch

data Config = Config
  { database :: String
  , help :: Bool
  , version :: Bool
  } deriving (Eq, Show)

initial :: Config
initial = Config
  { database = "hacana.sqlite"
  , help = False
  , version = False
  }

applyFlag :: Catch.MonadThrow m => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Database x -> pure config { database = x }
  Flag.Help x -> pure config { help = x }
  Flag.Version x -> pure config { version = x }

module Hacana.Type.Context where

import qualified Data.Pool as Pool
import qualified Database.SQLite.Simple as Sql
import qualified Hacana.Type.Config as Config
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls

data Context = Context
  { config :: Config.Config
  , manager :: Client.Manager
  , pool :: Pool.Pool Sql.Connection
  }

fromConfig :: Config.Config -> IO Context
fromConfig config = do
  manager <- Tls.newTlsManager
  pool <- Pool.newPool $ Pool.defaultPoolConfig
    (Sql.open $ Config.database config)
    Sql.close
    60 -- seconds
    8
  pure Context
    { config
    , manager
    , pool
    }

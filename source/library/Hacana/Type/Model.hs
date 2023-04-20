module Hacana.Type.Model where

import qualified Hacana.Type.Key as Key
import qualified Database.SQLite.Simple as Sql

data Model a = Model
  { key :: Key.Key a
  , value :: a
  } deriving (Eq, Show)

instance Sql.FromRow a => Sql.FromRow (Model a) where
  fromRow = do
    key <- Sql.field
    value <- Sql.fromRow
    pure Model { key, value }

instance Sql.ToRow a => Sql.ToRow (Model a) where
  toRow x = Sql.toRow [key x] <> Sql.toRow (value x)

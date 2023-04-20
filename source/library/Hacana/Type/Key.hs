module Hacana.Type.Key where

import qualified Data.Int as Int
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql

newtype Key a
  = Key Int.Int64
  deriving (Eq, Show)

fromInt64 :: Int.Int64 -> Key a
fromInt64 = Key

intoInt64 :: Key a -> Int.Int64
intoInt64 (Key x) = x

instance Sql.FromField (Key a) where
  fromField = fmap fromInt64 . Sql.fromField

instance Sql.ToField (Key a) where
  toField = Sql.toField . intoInt64

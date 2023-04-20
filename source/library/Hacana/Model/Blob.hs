module Hacana.Model.Blob where

import qualified Data.ByteString as ByteString
import qualified Hacana.Type.Key as Key
import qualified Hacana.Type.Model as Model
import qualified Hacana.Type.Hash as Hash
import qualified Database.SQLite.Simple as Sql

type Key = Key.Key Blob

type Model = Model.Model Blob

data Blob = Blob
  { contents :: ByteString.ByteString
  , hash :: Hash.Hash
  } deriving (Eq, Show)

instance Sql.FromRow Blob where
  fromRow = do
    contents <- Sql.field
    hash <- Sql.field
    pure Blob { contents, hash }

instance Sql.ToRow Blob where
  toRow x = Sql.toRow
    ( contents x
    , hash x
    )

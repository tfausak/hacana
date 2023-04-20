module Hacana.Model.HackageIndex where

import qualified Hacana.Type.Key as Key
import qualified Hacana.Model.Blob as Blob
import qualified Hacana.Type.Model as Model
import qualified Database.SQLite.Simple as Sql
import qualified Data.Time as Time

type Key = Key.Key HackageIndex

type Model = Model.Model HackageIndex

data HackageIndex = HackageIndex
  { blob :: Blob.Key
  , fetchedAt :: Time.UTCTime
  } deriving (Eq, Show)

instance Sql.FromRow HackageIndex where
  fromRow = do
    blob <- Sql.field
    fetchedAt <- Sql.field
    return HackageIndex { blob, fetchedAt }

instance Sql.ToRow HackageIndex where
  toRow x = Sql.toRow
    ( blob x
    , fetchedAt x
    )

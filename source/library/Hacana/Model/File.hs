module Hacana.Model.File where

import qualified Data.Time as Time
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Hacana.Model.Blob as Blob
import qualified Hacana.Type.Path as Path
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified System.Posix.Types as Posix

data File = File
  { blob :: Blob.Key
  , groupId :: Int
  , groupName :: Text.Text
  , ownerId :: Int
  , ownerName :: Text.Text
  , path :: Path.Path
  , permissions :: Tar.Entry.Permissions -- TODO: Use a custom type?
  , time :: Time.UTCTime
  } deriving (Eq, Show)

instance Sql.FromRow File where
  fromRow = do
    blob <- Sql.field
    groupId <- Sql.field
    groupName <- Sql.field
    ownerId <- Sql.field
    ownerName <- Sql.field
    path <- Sql.field
    permissions <- Posix.CMode <$> Sql.field
    time <- Sql.field
    pure File
      { blob
      , groupId
      , groupName
      , ownerId
      , ownerName
      , path
      , permissions
      , time
      }

instance Sql.ToRow File where
  toRow x = Sql.toRow
    ( blob x
    , groupId x
    , groupName x
    , ownerId x
    , ownerName x
    , path x
    , (\ (Posix.CMode y) -> y) $ permissions x
    , time x
    )

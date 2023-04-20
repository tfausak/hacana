module Hacana.Type.Path where

import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified System.FilePath.Posix as FilePath.Posix
import qualified System.FilePath.Windows as FilePath.Windows

newtype Path
  = Path (Seq.Seq Text.Text)
  deriving (Eq, Show)

fromSeq :: Seq.Seq Text.Text -> Path
fromSeq = Path

intoSeq :: Path -> Seq.Seq Text.Text
intoSeq (Path x) = x

fromFilePath :: FilePath -> Path
fromFilePath = fromSeq . Seq.fromList . fmap Text.pack . FilePath.Windows.splitDirectories

intoFilePath :: Path -> FilePath
intoFilePath = FilePath.Posix.joinPath . Foldable.toList . fmap Text.unpack . intoSeq

instance Sql.FromField Path where
  fromField = fmap fromFilePath . Sql.fromField

instance Sql.ToField Path where
  toField = Sql.toField . intoFilePath

fromTarPath :: Tar.Entry.TarPath -> Path
fromTarPath = fromFilePath . Tar.Entry.fromTarPath

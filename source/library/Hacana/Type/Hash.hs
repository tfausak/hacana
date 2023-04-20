module Hacana.Type.Hash where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteArray as ByteArray

newtype Hash
  = Hash (Crypto.Digest Crypto.MD5)
  deriving (Eq, Show)

fromDigest :: Crypto.Digest Crypto.MD5 -> Hash
fromDigest = Hash

intoDigest :: Hash -> Crypto.Digest Crypto.MD5
intoDigest (Hash x) = x

instance Sql.FromField Hash where
  fromField field = do
    byteString <- Sql.fromField @ByteString.ByteString field
    case Crypto.digestFromByteString byteString of
      Nothing -> fail "invalid Hash"
      Just x -> pure $ fromDigest x

instance Sql.ToField Hash where
  toField = Sql.toField @ByteString.ByteString . ByteArray.convert . intoDigest

new :: ByteString.ByteString -> Hash
new = fromDigest . Crypto.hash

module Hacana.Type.PackageName where

import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Hacana.Extra.Either as Either

newtype PackageName
  = PackageName Cabal.PackageName
  deriving (Eq, Ord, Show)

fromCabal :: Cabal.PackageName -> PackageName
fromCabal = PackageName

intoCabal :: PackageName -> Cabal.PackageName
intoCabal (PackageName x) = x

instance Sql.FromField PackageName where
  fromField field = do
    string <- Sql.fromField field
    Either.fail $ fromString string

instance Sql.ToField PackageName where
  toField = Sql.toField . intoString

fromString :: String -> Either String PackageName
fromString = fmap fromCabal . Cabal.eitherParsec

intoString :: PackageName -> String
intoString = Cabal.prettyShow . intoCabal

module Hacana.Type.VersionNumber where

import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Hacana.Extra.Either as Either

newtype VersionNumber
  = VersionNumber Cabal.Version
  deriving (Eq, Ord, Show)

fromCabal :: Cabal.Version -> VersionNumber
fromCabal = VersionNumber

intoCabal :: VersionNumber -> Cabal.Version
intoCabal (VersionNumber x) = x

instance Sql.FromField VersionNumber where
  fromField field = do
    string <- Sql.fromField field
    Either.fail $ fromString string

instance Sql.ToField VersionNumber where
  toField = Sql.toField . intoString

fromString :: String -> Either String VersionNumber
fromString = fmap fromCabal . Cabal.eitherParsec

intoString :: VersionNumber -> String
intoString = Cabal.prettyShow . intoCabal

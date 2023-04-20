module Hacana.Type.Constraint where

import qualified Distribution.Types.VersionRange as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Hacana.Extra.Either as Either

newtype Constraint
  = Constraint Cabal.VersionRange
  deriving (Eq, Ord, Show)

fromCabal :: Cabal.VersionRange -> Constraint
fromCabal = Constraint

intoCabal :: Constraint -> Cabal.VersionRange
intoCabal (Constraint x) = x

instance Sql.FromField Constraint where
  fromField field = do
    string <- Sql.fromField field
    Either.fail $ fromString string

instance Sql.ToField Constraint where
  toField = Sql.toField . intoString

fromString :: String -> Either String Constraint
fromString = fmap fromCabal . Cabal.eitherParsec

intoString :: Constraint -> String
intoString = Cabal.prettyShow . intoCabal

any :: Constraint
any = fromCabal Cabal.anyVersion

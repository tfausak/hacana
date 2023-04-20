module Hacana.Type.Revision where

import qualified Numeric.Natural as Natural
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql

newtype Revision
  = Revision Natural.Natural
  deriving (Eq, Ord, Show)

fromNatural :: Natural.Natural -> Revision
fromNatural = Revision

intoNatural :: Revision -> Natural.Natural
intoNatural (Revision x) = x

instance Sql.FromField Revision where
  fromField field = do
    integer <- Sql.fromField field
    case integerToNatural integer of
      Nothing -> fail "invalid revision"
      Just natural -> pure $ fromNatural natural

instance Sql.ToField Revision where
  toField = Sql.toField . naturalToInteger . intoNatural

naturalToInteger :: Natural.Natural -> Integer
naturalToInteger = fromIntegral

integerToNatural :: Integer -> Maybe Natural.Natural
integerToNatural x = if x < 0 then Nothing else Just $ fromIntegral x

zero :: Revision
zero = fromNatural 0

increment :: Revision -> Revision
increment = fromNatural . succ . intoNatural

intoString :: Revision -> String
intoString = show . intoNatural

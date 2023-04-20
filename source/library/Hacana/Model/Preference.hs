module Hacana.Model.Preference where

import qualified Database.SQLite.Simple as Sql
import qualified Hacana.Type.Constraint as Constraint
import qualified Hacana.Type.Key as Key
import qualified Hacana.Type.Model as Model
import qualified Hacana.Type.PackageName as PackageName

type Key = Key.Key Preference

type Model = Model.Model Preference

data Preference = Preference
  { constraint :: Constraint.Constraint
  , package :: PackageName.PackageName
  } deriving (Eq, Show)

instance Sql.FromRow Preference where
  fromRow = do
    constraint <- Sql.field
    package <- Sql.field
    return Preference { constraint, package }

instance Sql.ToRow Preference where
  toRow x = Sql.toRow
    ( constraint x
    , package x
    )

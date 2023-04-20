module Hacana.Type.Flag where

import qualified System.Console.GetOpt as GetOpt

data Flag
  = Database String
  | Help Bool
  | Version Bool
  deriving (Eq, Show)

optDescrs :: [GetOpt.OptDescr Flag]
optDescrs =
  [ GetOpt.Option ['h', '?'] ["help"] (GetOpt.NoArg $ Help True) "Shows this help message."
  , GetOpt.Option [] ["no-help"] (GetOpt.NoArg $ Help False) ""
  , GetOpt.Option [] ["version"] (GetOpt.NoArg $ Version True) "Shows the version number."
  , GetOpt.Option [] ["no-version"] (GetOpt.NoArg $ Version False) ""
  , GetOpt.Option [] ["database"] (GetOpt.ReqArg Database "FILE") "Specifies the database file."
  ]

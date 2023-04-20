module HacanaSpec where

import qualified Test.Hspec as Hspec
import qualified Hacana.Extra.EitherSpec
import qualified Hacana.Extra.MaybeSpec

spec :: Hspec.Spec
spec = do
  Hacana.Extra.EitherSpec.spec
  Hacana.Extra.MaybeSpec.spec

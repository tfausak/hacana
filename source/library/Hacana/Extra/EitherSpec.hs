module Hacana.Extra.EitherSpec where

import qualified Test.Hspec as Hspec
import qualified Hacana.Extra.Either as Either
import qualified Control.Monad.Catch as Catch

spec :: Hspec.Spec
spec = Hspec.describe "Hacana.Extra.Either" $ do
  Hspec.describe "fail" $ do
    Hspec.it "fails on left" $ do
      Either.fail (Left "problem") `Hspec.shouldThrow` (== userError "problem")

    Hspec.it "returns on right" $ do
      Either.fail (Right ()) `Hspec.shouldReturn` ()

  Hspec.describe "throw" $ do
    Hspec.it "throws a left" $ do
      Either.throw (Left Problem) `Hspec.shouldThrow` (== Problem)

    Hspec.it "returns a right" $ do
      Either.throw (Right @Problem ()) `Hspec.shouldReturn` ()

data Problem
  = Problem
  deriving (Eq, Show)

instance Catch.Exception Problem

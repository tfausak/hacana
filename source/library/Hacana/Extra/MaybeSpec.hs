module Hacana.Extra.MaybeSpec where

import qualified Test.Hspec as Hspec
import qualified Hacana.Extra.Maybe as Maybe

spec :: Hspec.Spec
spec = Hspec.describe "Hacana.Extra.Maybe" $ do
  Hspec.describe "note" $ do
    Hspec.it "turns nothing into left" $ do
      Maybe.note () (Nothing @Char) `Hspec.shouldBe` Left ()

    Hspec.it "turns just into right" $ do
      Maybe.note () (Just 'a') `Hspec.shouldBe` Right 'a'

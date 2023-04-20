module Hacana.TestSuite where

import qualified Test.Hspec as Hspec
import qualified HacanaSpec

testSuite :: IO ()
testSuite = Hspec.hspec HacanaSpec.spec

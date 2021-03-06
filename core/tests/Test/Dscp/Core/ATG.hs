module Test.Dscp.Core.ATG where

import Dscp.Core (Subject)
import qualified Dscp.Core as Core
import Dscp.Util (Id)

import Test.Hspec

pathFromTo :: Id Subject -> Id Subject -> Bool
pathFromTo = Core.hasPathFromTo Core.activityTypeGraphIndexed

(~~>) :: Id Subject -> Id Subject -> Expectation
a ~~> b = pathFromTo a b `shouldBe` True

(!~>) :: Id Subject -> Id Subject -> Expectation
a !~> b = pathFromTo a b `shouldBe` False

spec_validPaths :: Spec
spec_validPaths = describe "Valid path queries" $ do
    specify "Mathematics is an ancestor of Logic" $ 1 ~~> 5
    specify "Algebra isn't ancestor of Mathematics" $ 8 !~> 1
    specify "Mathematics isn't ancestor of Engineering" $ 1 !~> 6
    specify "Mathematics is an ancestor of pi-calculus" $ 1 ~~> 9
    specify "CS is an ancestor of pi-calculus" $ 2 ~~> 9

spec_invalidPaths :: Spec
spec_invalidPaths = describe "Invalid path queries" $ do
    specify "First subject isn't in the ATG #1" $ 0 !~> 5
    specify "First subject isn't in the ATG #2" $ 17 !~> 3
    specify "Second subject isn't in the ATG" $ 4 !~> 19
    specify "Both subjects aren't in the ATG" $ 123 !~> 881

module LifeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Life
import qualified Data.Set as S

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "step" $ do
    it "does nothing for an empty CellSet" $
      step S.empty `shouldBe` S.empty
    it "removes a cell with fewer than two neighbors" $
      step (S.fromList [(0,0)]) `shouldSatisfy` S.null
    it "removes a cell with more than three neighbors" $
      step (S.fromList [(0,0), (1,1), (1,-1), (-1, 1), (-1,-1)]) `shouldBe` S.fromList [(0,1), (0, -1), (-1, 0), (1, 0)]
    it "keeps a cell with two neighbors" $
      step (S.fromList [(0,0), (1,1), (-1,-1)]) `shouldBe` S.fromList [(0,0)]
    it "keeps a cell with three neighbors" $
      step (S.fromList [(0,0), (1,1), (-1, 1), (-1,-1)]) `shouldBe` S.fromList [(0,0), (-1, 0), (0, 1)]
    it "spawns a cell with three neighbors" $
      step (S.fromList [(1,1), (-1, 1), (-1,-1)]) `shouldBe` S.fromList [(0,0)]

  describe "isNeighbor" $ do
    it "is not its own neighbor" $
      property (\x y -> not $ isNeighbor (x, y) (x, y))
    it "is a neighbor of a cell that is 1 away" $
      isNeighbor (0, 0) (0, 1) `shouldBe` True
    it "is a neighbor of a cell that is 1 away in each axis" $
      isNeighbor (0, 0) (1, 1) `shouldBe` True
    it "is not a neighbor of a cell that is more than 1 away" $
      isNeighbor (0, 0) (0, 2) `shouldBe` False

  describe "generateNeighbors" $ do
    it "generates no neighbors for an empty set" $
      generateNeighbors S.empty `shouldSatisfy` S.null
    it "generates a cell with three neighbors" $
      generateNeighbors (S.fromList [(0,1), (1, 0), (1,1)]) `shouldBe` S.fromList [(0,0)]
    it "doesn't generate a cell that is already live" $
      generateNeighbors (S.fromList [(0,0), (0,1), (1, 0), (1,1)]) `shouldSatisfy` S.null

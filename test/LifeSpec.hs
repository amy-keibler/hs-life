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
  describe "board" $ do
    it "should fail for an zero sized board" $
      mkBoard 0 0 (S.fromList [(0,0)]) `shouldBe` Left "Board must be at least 3x3"
    it "should fail for negative sizes" $
      mkBoard (-1) (-1) (S.fromList [(0,0)]) `shouldBe` Left "Board must be at least 3x3"
    it "should fail for sizes less than 3x3" $
      mkBoard 2 2 (S.fromList [(0,0)]) `shouldBe` Left "Board must be at least 3x3"

  describe "runBoard" $ do
    it "should always have the first set of cells, even if empty" $
      runBoard (testBoard S.empty) `shouldSatisfy` ((1 ==) . length)
    it "should stop once the board is empty" $
      runBoard (testBoard (S.fromList [(0,0),
                                       (1,1),
                                       (63,63)])) `shouldBe` [S.fromList [(0,0),
                                                                          (1,1),
                                                                          (63,63)],
                                                              S.fromList [(0,0)]]

  describe "step" $ do
    it "does nothing for an empty CellSet" $
      step testDims S.empty `shouldBe` S.empty
    it "removes a cell with fewer than two neighbors" $
      step testDims (S.fromList [(0,0)]) `shouldSatisfy` S.null
    it "removes a cell with more than three neighbors" $
      step testDims (S.fromList [(0,0),
                                 (1,1),
                                 (1,63),
                                 (63, 1),
                                 (63,63)]) `shouldBe` S.fromList [(0,1),
                                                                  (0, 63),
                                                                  (63, 0),
                                                                  (1, 0)]
    it "keeps a cell with two neighbors" $
      step testDims (S.fromList [(0,0),
                                 (1,1),
                                 (63,63)]) `shouldBe` S.fromList [(0,0)]
    it "keeps a cell with three neighbors" $
      step testDims (S.fromList [(0,0),
                                 (1,1),
                                 (63, 1),
                                 (63,63)]) `shouldBe` S.fromList [(0,0),
                                                                  (63, 0),
                                                                  (0, 1)]
    it "spawns a cell with three neighbors" $
      step testDims (S.fromList [(1,1),
                                 (63, 1),
                                 (63,63)]) `shouldBe` S.fromList [(0,0)]

  describe "isNeighbor" $ do
    it "is not its own neighbor" $
      property (\x y -> not $ isNeighbor testDims (x, y) (x, y))
    it "is a neighbor of a cell that is 1 away" $
      isNeighbor testDims (0, 0) (0, 1) `shouldBe` True
    it "is a neighbor of a cell that is 1 away in each axis" $
      isNeighbor testDims (0, 0) (1, 1) `shouldBe` True
    it "is not a neighbor of a cell that is more than 1 away" $
      isNeighbor testDims (0, 0) (0, 2) `shouldBe` False
    it "is a neighbor to cells that wrap" $
      isNeighbor testDims (0, 0) (63, 63) `shouldBe` True

  describe "generateNeighbors" $ do
    it "generates no neighbors for an empty set" $
      generateNeighbors testDims S.empty `shouldSatisfy` S.null
    it "generates a cell with three neighbors" $
      generateNeighbors testDims (S.fromList [(0,1), (1, 0), (1,1)]) `shouldBe` S.fromList [(0,0)]
    it "doesn't generate a cell that is already live" $
      generateNeighbors testDims (S.fromList [(0,0), (0,1), (1, 0), (1,1)]) `shouldSatisfy` S.null

testBoard :: CellSet -> Board
testBoard = either error id . mkBoard 64 64

testDims :: Dimensions
testDims = either error id $ mkDims 64 64

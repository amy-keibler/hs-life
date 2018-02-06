{-# LANGUAGE OverloadedStrings #-}
module Life (step
            , Cell
            , CellSet
            , Board
            , Dimensions
            , mkBoard
            , mkDims
            , runBoard
            , numNeighbors
            , isNeighbor
            , generateNeighbors) where

import Data.Text as T
import qualified Data.Set as S

type Cell = (Int, Int)
type CellSet = S.Set Cell

data Dimensions = Dimensions { width :: Int, height :: Int } deriving (Eq, Show)

data Board = Board Dimensions CellSet deriving (Eq, Show)

mkDims :: Int -> Int -> Either Text Dimensions
mkDims w h = if w < 3 || h < 3
  then Left "Board must be at least 3x3"
  else Right  Dimensions{ width = w, height = h}

mkBoard :: Int -> Int -> CellSet -> Either Text Board
mkBoard w h c = do
  dims <- mkDims w h
  return (Board dims (S.map (wrapCell dims) c))

runBoard :: Board -> [CellSet]
runBoard (Board dims cellSet) = cellSet : genNext dims cellSet

genNext :: Dimensions -> CellSet -> [CellSet]
genNext dims cells | S.null nextCellS = []
                   | otherwise = nextCellS : genNext dims nextCellS
  where nextCellS = step dims cells

step :: Dimensions -> CellSet -> CellSet
step dims cellSet = S.union filteredCells spawnedCells
  where filteredCells = S.filter (validNumNeighbors dims cellSet) cellSet
        spawnedCells = generateNeighbors dims cellSet

validNumNeighbors :: Dimensions -> CellSet -> Cell -> Bool
validNumNeighbors dims cellSet cell = neighborCount == 3 || neighborCount == 2
  where neighborCount = numNeighbors dims cell cellSet

numNeighbors :: Dimensions -> Cell -> CellSet -> Int
numNeighbors dims cell cellSet = S.size $ neighbors dims cell cellSet

neighbors :: Dimensions -> Cell -> CellSet -> CellSet
neighbors dims cell = S.filter (isNeighbor dims cell)

isNeighbor :: Dimensions -> Cell -> Cell -> Bool
isNeighbor dims (x0, y0) (x1, y1)
  | x0 == x1 && y0 == y1 = False
  | axisNeighbor (width dims) x0 x1 && axisNeighbor (height dims) y0 y1 = True
  | otherwise = False
  where axisNeighbor size a a' = abs (a' - a) <= 1 || abs (a' - a) >= (size - 1)

generateNeighbors :: Dimensions -> CellSet -> CellSet
generateNeighbors dims current = S.filter threeNeighbors potentialDead
  where potentialDead = (S.\\ current) $ S.foldl (allNeighbors dims) S.empty current
        threeNeighbors = (3 ==) . flip (numNeighbors dims) current

allNeighbors :: Dimensions -> CellSet -> Cell -> CellSet
allNeighbors dims cellSet cell = neighborsOf dims cell `S.union` cellSet

neighborsOf :: Dimensions -> Cell -> CellSet
neighborsOf dims (x, y) = S.fromList $ wrapCell dims <$> [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                                 (x - 1, y), (x + 1, y),
                                 (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

wrapCell :: Dimensions -> Cell -> Cell
wrapCell (Dimensions w h) (x, y) = (x `mod` w, y `mod` h)

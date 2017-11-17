module Life (step
            , Cell
            , CellSet
            , numNeighbors
            , isNeighbor
            , generateNeighbors) where

import qualified Data.Set as S

type Cell = (Int, Int)
type CellSet = S.Set Cell

step :: CellSet -> CellSet
step cellSet = S.union filteredCells spawnedCells
  where filteredCells = S.filter (validNumNeighbors cellSet) cellSet
        spawnedCells = generateNeighbors cellSet

validNumNeighbors :: CellSet -> Cell -> Bool
validNumNeighbors cellSet cell = neighborCount == 3 || neighborCount == 2
  where neighborCount = numNeighbors cell cellSet

numNeighbors :: Cell -> CellSet -> Int
numNeighbors cell cellSet = S.size $ neighbors cell cellSet

neighbors :: Cell -> CellSet -> CellSet
neighbors cell = S.filter (cell `isNeighbor`)

isNeighbor :: Cell -> Cell -> Bool
(x0, y0) `isNeighbor` (x1, y1)
  | x0 == x1 && y0 == y1 = False
  | axisNeighbor x0 x1 && axisNeighbor y0 y1 = True
  | otherwise = False
  where axisNeighbor a a' = abs (a' - a) <= 1

generateNeighbors :: CellSet -> CellSet
generateNeighbors current = S.filter threeNeighbors potentialDead
  where potentialDead = (S.\\ current) $ S.foldl deadNeighbors S.empty current
        threeNeighbors = (3 ==) . flip numNeighbors current

deadNeighbors :: CellSet -> Cell -> CellSet
deadNeighbors cellSet cell = neighborsOf cell `S.union` cellSet

neighborsOf :: Cell -> CellSet
neighborsOf (x, y) = S.fromList [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                                 (x - 1, y), (x + 1, y),
                                 (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]


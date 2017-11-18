module Main where

import Life

import qualified Data.Set as S

import Codec.Picture
import Codec.Picture.Gif

main :: IO ()
main = either print id $ boardOrError
  where boardOrError = (mkBoard boardSize boardSize (glider 0 0)) >>= printBoard

printBoard :: Board -> Either String (IO ())
printBoard board = writeGifImages "/tmp/glider_animated.gif" LoopingForever gifFrames
  where frames = take maxFrames $ runBoard board
        pixelFn cells x y = if S.member (x `quot` cellSize, y `quot` cellSize) cells then 0 else 255
        cellsToImage cells = generateImage (pixelFn cells) imageSize imageSize
        images = cellsToImage <$> frames
        gifFrames = (\x -> (greyPalette, 10, x)) <$> images

boardSize :: Int
boardSize = 16

cellSize :: Int
cellSize = 16

imageSize :: Int
imageSize = boardSize * cellSize

maxFrames :: Int
maxFrames = 64

glider :: Int -> Int -> CellSet
glider offX offY = S.fromList $ shiftCell <$> [(1,0), (2,1), (0,2), (1,2), (2,2)]
  where shiftCell (x, y) = (x + offX, y + offY)

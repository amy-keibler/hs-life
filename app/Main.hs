module Main where

import Life

import qualified Data.Set as S

import Codec.Picture
import Codec.Picture.Gif

main :: IO ()
main = either print printBoard $ mkBoard imageSize imageSize glider

printBoard :: Board -> IO ()
printBoard board = writeGifImage "/tmp/glider.gif" image
  where image = generateImage (\x y -> if S.member (x, y) (cells board) then 0 else 255) imageSize imageSize

imageSize :: Int
imageSize = 64

maxFrames :: Int
maxFrames = 60

glider :: CellSet
glider = S.fromList [(1,0), (2,1), (3,0), (3,1), (3,2)]

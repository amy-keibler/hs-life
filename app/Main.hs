module Main where

import Life
import Config

import qualified Data.Set as S
import Data.Bifunctor
import Data.Text as T hiding (take)
import Data.Vector as V hiding (take)
import Data.ByteString.Lazy as B (readFile, writeFile, ByteString)
import Data.Csv

import System.IO (hPutStrLn, stderr)
import Control.Monad

import Codec.Picture
import Codec.Picture.Gif

main :: IO ()
main = do
  options <- parseOptions
  input <- parseInput options
  let board = join $ constructBoard <$> options <*> input
      gif = join $ encodeGif <$> options <*> board
  either (hPutStrLn stderr . T.unpack) (writeGif options) gif

parseInput :: Either T.Text Options -> IO (Either T.Text CellSet)
parseInput (Left e) = return $ Left e
parseInput (Right options)= do
  contents <- B.readFile $ T.unpack $ oInputFilename options
  return $ bimap T.pack (S.fromList . V.toList) (decode NoHeader contents)

constructBoard :: Options -> CellSet -> Either T.Text Board
constructBoard options = mkBoard (oBoardSize options) (oBoardSize options)

encodeGif :: Options -> Board -> Either T.Text B.ByteString
encodeGif options board = first T.pack (encodeGifImages LoopingForever gifFrames)
  where imageSize = oBoardSize options * oCellSize options
        frames = take (oMaxFrames options) $ runBoard board
        pixelFn cells x y = if S.member (x `quot` oCellSize options, y `quot` oCellSize options) cells
          then 0
          else 255
        cellsToImage cells = generateImage (pixelFn cells) imageSize imageSize
        images = cellsToImage <$> frames
        gifFrames = (\x -> (greyPalette, 10, x)) <$> images

writeGif :: Either T.Text Options -> B.ByteString -> IO ()
writeGif (Left e) _ = hPutStrLn stderr $ T.unpack e
writeGif (Right options) gif = B.writeFile (T.unpack $ oOutputFilename options) gif

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Config (Options(..)
              , parseOptions)where

import Data.Monoid
import Data.Text
import Options.Applicative

data Options = Options
  { oBoardSize :: Int
  , oCellSize :: Int
  , oMaxFrames :: Int
  , oInputFilename :: Text
  , oOutputFilename :: Text
  } deriving (Show, Eq)

data PartialOptions = PartialOptions
  { poBoardSize :: Last Int
  , poCellSize :: Last Int
  , poMaxFrames :: Last Int
  , poInputFilename :: Last Text
  , poOutputFilename :: Last Text
  } deriving (Show, Eq)

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty mempty mempty
  mappend l r = PartialOptions
    { poBoardSize = poBoardSize l <> poBoardSize r
    , poCellSize = poCellSize l <> poCellSize r
    , poMaxFrames = poMaxFrames l <> poMaxFrames r
    , poInputFilename = poInputFilename l <> poInputFilename r
    , poOutputFilename = poOutputFilename l <> poOutputFilename r
    }

lastToEither :: Text -> Last a -> Either Text a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

mkOptions :: PartialOptions -> Either Text Options
mkOptions PartialOptions {..} = do
  oBoardSize <- lastToEither "Missing board size" poBoardSize
  oCellSize <- lastToEither "Missing cell draw size" poCellSize
  oMaxFrames <- lastToEither "Missing the maximum number of animation frames" poMaxFrames
  oInputFilename <- lastToEither "Missing input csv filename" poInputFilename
  oOutputFilename <- lastToEither "Missing output gif filename" poOutputFilename
  return Options {..}

parseOptions :: IO (Either Text Options)
parseOptions = do
  cmdlineOptions <- execParser $ info partialOptionsParser mempty
  let combinedOptions = defaultPartialOptions
        <> cmdlineOptions
  return $ mkOptions combinedOptions

-- Defaults Options
defaultPartialOptions :: PartialOptions
defaultPartialOptions =  mempty
  { poBoardSize = pure 16
  , poCellSize = pure 16
  , poMaxFrames = pure 64
  , poOutputFilename = pure "/tmp/life.gif"
  }

-- Command Line Options
lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

partialOptionsParser :: Parser PartialOptions
partialOptionsParser
   =  PartialOptions
  <$> lastOption (option auto (long  "board-size"))
  <*> lastOption (option auto (long  "cell-size"))
  <*> lastOption (option auto (long  "max-frames"))
  <*> lastOption (option str  (long  "input-filename"))
  <*> lastOption (option str  (long  "output-filename"))

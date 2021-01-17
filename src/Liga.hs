{-# LANGUAGE OverloadedStrings #-}

module Liga where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Club = Club {
    name :: Text,
    capacity :: Int,
    europeanTrophies :: Int,
    domesticTrophies :: Int,
    marketValue :: Double,
    internationalPlayers :: Int
} deriving (Show)

parseLine :: Text -> Maybe Club
parseLine s = case fields of
    [name, capacity, europe, domestic, market, international] ->
        Just $ Club (T.pack name) (read capacity) (read europe) (read domestic) (read market) (read international)
    _ -> Nothing
    where fields = map (T.unpack . T.strip) . T.splitOn "," $ s

parseCSVFile :: FilePath -> IO [Club]
parseCSVFile path = mapMaybe parseLine . tail . T.lines <$> T.IO.readFile path
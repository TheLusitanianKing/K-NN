{-# LANGUAGE OverloadedStrings #-}

module Liga where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Text.Read (readMaybe)

-- | Data structure representing a successfully parsed club
data Club = Club {
    name :: Text,
    capacity :: Int,
    europeanTrophies :: Int,
    domesticTrophies :: Int,
    marketValue :: Double,
    internationalPlayers :: Int
} deriving (Show)

-- | Parsing a CSV line of a club
parseLine :: Text -> Maybe Club
parseLine s = do
    case map (T.unpack . T.strip) . T.splitOn "," $ s of
        [sname, tcapacity, teurope, tdomestic, tmarket, tinternational] -> do
            capacity <- readMaybe tcapacity
            europe <- readMaybe teurope
            domestic <- readMaybe tdomestic
            market <- readMaybe tmarket
            international <- readMaybe tinternational
            return $ Club (T.pack sname) capacity europe domestic market international
        _ -> Nothing

-- | Parsing a CSV file of multiple clubs
parseCSVFile :: FilePath -> IO [Club]
parseCSVFile path = mapMaybe parseLine . T.lines <$> T.IO.readFile path
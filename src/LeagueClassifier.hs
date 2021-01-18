{-# LANGUAGE OverloadedStrings #-}

module LeagueClassifier where

import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Text.Read (readMaybe)

-- | Data structure representing a successfully parsed club
data Club = Club {
    name :: Text,
    capacity :: Double,
    europeanTrophies :: Double,
    domesticTrophies :: Double,
    marketValue :: Double,
    internationalPlayers :: Double
} deriving (Eq, Show)

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

-- | Weighted Euclidean distance between 2 clubs
-- TODO: Make it more generic and less verbose
euclideanDistance :: Club -> Club -> Double
euclideanDistance c1 c2 = sqrt $ sum distances
    where distances :: [Double]
          distances = [
                (1 ^ 2) * (capacity c1 - capacity c2) ^ 2,
                (3 ^ 2) * (europeanTrophies c1 - europeanTrophies c2) ^ 2,
                (3 ^ 2) * (domesticTrophies c1 - domesticTrophies c2) ^ 2,
                (4 ^ 2) * (marketValue c1 - marketValue c2) ^ 2,
                (1 ^ 2) * (internationalPlayers c1 - internationalPlayers c2) ^ 2
            ]

-- | List the nearest neighbours
nearestNeighbours :: Int              -- how many neighbours (the K in K-NN)
                  -> Club             -- the club to compare
                  -> [Club]           -- all the other clubs
                  -> [(Text, Double)] -- the K nearest neighbours with their euclidean distance (sorted)
nearestNeighbours k c = take k
                      . sortBy (\(_, x1) (_, x2) -> compare x1 x2)
                      . map (\x -> (name x, euclideanDistance c x))
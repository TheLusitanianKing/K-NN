{-# LANGUAGE OverloadedStrings #-}

-- | Generic K-NN classifier
module Classifier where

import Data.List (transpose, sortBy)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Text.Read (readMaybe)

-- | Some type aliases
type Weigh  = Double
type Value  = Double
type Entity = (Text, [Value])

-- | Algebraic data type representing the classifier (based on the input file)
data Classifier = Classifier {
    features :: [(Text, Weigh)],
    entities :: [Entity]
}

-- | Algebraic data type representing the target values that need to be classified
data Target = Target {
    featureToClassify :: Text,
    targets :: [Entity]
}

-- | Print the classifier my way
instance Show Classifier where
    show c = (show . length $ features c)
           ++ " features and associated weight: \n"
           ++ (concatMap (\(t, w) -> indent 4 ++ (T.unpack t) ++ ": " ++ show w ++ "\n") (features c))
           ++ (show . length $ entities c)
           ++ " entities: \n"
           ++ (concatMap (\(t, vs) -> indent 4 ++ (T.unpack t) ++ ": " ++ show vs ++ "\n") (entities c))

-- | Helper function to give an simple blank space for indentation purpose
indent :: Int -> String
indent x = concat (replicate x " ")

-- | Parsing a CSV file
parseCSVFile :: FilePath -> IO (Maybe Classifier)
parseCSVFile path = parse . map (T.splitOn ",") . T.lines <$> T.IO.readFile path
    where parse :: [[Text]] -> Maybe Classifier
          parse (names:weighs:entities) =
              let fs = zipWith (\name weigh -> (T.strip name, read . T.unpack $ weigh)) (tail names) (tail weighs) in
              return $ Classifier {
                  features = fs,
                  entities = map parseEntity entities
              }
          parse _ = Nothing
          parseEntity :: [Text] -> Entity
          parseEntity (name:values) =
              (T.strip name, mapMaybe (readMaybe . T.unpack) values)
          parseEntity _             = error "Malformed entity value..."

-- | Scaling all values between -1 and 1
scaling :: Classifier -> Classifier
scaling c = c { entities = scaledEntities }
    where scaledEntities :: [Entity]
          scaledEntities = map (\(t, vs) -> (t, zipWith (\(min, max) v -> (v - min) / (max - min)) minmax vs)) (entities c)
          -- minmax being for each feature the tuple (minimum value, maximum value)
          minmax = map (\vs -> (minimum vs, maximum vs)) . transpose . map snd $ entities c

-- | Weighted Euclidean distance between 2 entities
euclideanDistance :: Classifier -> Entity -> Entity -> Double
euclideanDistance c e1 e2 = sqrt . sum $ weightedDists
    where weightedDists = zipWith (\(_, weigh) x -> weigh * x) (features c) distances
          distances = zipWith (\v1 v2 -> (v2 - v1) ^ 2) vs1 vs2
          (_, vs1) = e1
          (_, vs2) = e2

-- | List the nearest neighbours
nearestNeighbours :: Int         -- how many neighbours (the K in K-NN)
                  -> Entity      -- the entity that will be compared
                  -> Classifier  -- the classifier to use
                  -> [Entity]    -- the K nearest neighbours with their euclidean distance (sorted)
nearestNeighbours k e c = take k -- take K nearest
                        . map fst -- take entities, ignore distances
                        . sortBy (\(_, d) (_, d') -> compare d d') -- sort by distance
                        . map (\x -> (x, euclideanDistance c e x)) -- euclidean distance with all entities
                        $ entities c
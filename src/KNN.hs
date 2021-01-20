{-# LANGUAGE OverloadedStrings #-}

-- | K-NN algorithm
module KNN where

import Data.List (findIndex, transpose, sortBy)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Text.Read (readMaybe)

-- | Type alias for the values
type Value = Double

-- | Algebraic data type representing an object
data Object = Object {
    name      :: Text,       -- ^ its name
    variables :: [Value],    -- ^ its variable (unscaled)
    object    :: Maybe Value -- ^ its class/variable to predict (if exists, already classified)
}

-- | Algebraic data type representing the classifier (based on the input file)
data Classifier = Classifier {
    weighs  :: [Value],
    objects :: [Object]
}

-- | Retrieving classified and unclassified objects of the classifier
classified, unclassified :: Classifier -> [Object]
classified = filter (isJust . object) . objects
unclassified = filter (isNothing . object) . objects

-- | Custom print for the classifier
instance Show Classifier where
    show c = "Weighs: "
           ++ show (weighs c)
           ++ "\n" ++ "Objects: " ++ "\n"
           ++ concatMap
                (\o ->
                    indent 4 ++ T.unpack (name o) ++ ": " ++ show (variables o)
                    ++ " -> "
                    ++ (case object o of
                            Nothing -> "not yet classified"
                            Just v  -> "classified as " ++ show v
                        )
                    ++ "\n"
                )
                (objects c)

-- | Helper function to give an simple blank space for indentation purpose
indent :: Int -> String
indent x = concat (replicate x " ")

-- | Parsing a CSV file
parseCSVFile :: Text          -- ^ content of the CSV file
             -> Text          -- ^ name of the class/variable to predict
             -> Classifier -- ^ completed classifier
parseCSVFile content target = parse . map (map T.strip . T.splitOn ",") . T.lines $ content
    where parse :: [[Text]] -> Classifier
          parse ((_:columns):weighs:objects) =
              case (==target) `findIndex` columns of
                  Nothing -> error "Could not find the class/variable to predict in the CSV."
                  Just i  -> Classifier {
                      -- to make it easier, we suppose the class will always have a - indicating a missing value
                      weighs  = map (read . T.unpack) (filter (/="-") weighs),
                      objects = map (parseObject i) objects
                  }
          parse _ = error "Missing data in the parsed CSV."
          parseObject :: Int -> [Text] -> Object
          parseObject i (name:values) =
              let v  = values !! i                               -- retrieving class value
                  vs = let (a, _:b) = splitAt i values in a ++ b -- removing class from the variables
              in Object {
                  name = T.strip name,
                  variables = map (read . T.unpack) vs,
                  object = if v == "-" then Nothing else return $ (read . T.unpack) v
                }
          parseObject _ _ = error "Malformed entity value..."

-- | Scaling all values between -1 and 1
scaling :: Classifier -> Classifier
scaling c = c { objects = scaledObjects }
    where scaledObjects :: [Object]
          scaledObjects =
              map (\o -> o { variables = zipWith minmaxScaling minmax (variables o)})
                $ (objects c)
          -- minmax being for each column of variables the tuple (minimum value, maximum value)
          minmax = map (\vs -> (minimum vs, maximum vs)) . transpose . map variables $ objects c

-- | Rescaling a value (min-max normalization)
minmaxScaling :: Fractional a => (a, a) -> a -> a
minmaxScaling (min, max) x = (x - min) / (max - min)

-- | Weighted Euclidean distance between 2 entities
euclideanDistance :: [Double] -> Object -> Object -> Double
euclideanDistance ws o1 o2 = sqrt . sum $ weightedDists
    where weightedDists = zipWith (*) ws distances
          distances = zipWith (\v1 v2 -> (v2 - v1) ^ 2) (variables o1) (variables o2)

-- | List the nearest neighbours
-- nearestNeighbours :: Int         -- how many neighbours (the K in K-NN)
--                   -> Object      -- the entity that will be compared
--                   -> Classifier  -- the classifier to be used
--                   -> [Object]    -- the K nearest neighbours with their euclidean distance (sorted)
-- nearestNeighbours k e c = take k -- take K nearest
--                         . map fst -- take entities, ignore distances
--                         . sortBy (\(_, d) (_, d') -> compare d d') -- sort by distance
--                         . map (\x -> (x, euclideanDistance (weighs c) e x)) -- euclidean distance with all entities
--                         $ classified c
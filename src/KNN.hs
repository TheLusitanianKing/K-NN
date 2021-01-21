{-# LANGUAGE OverloadedStrings #-}

-- | K-NN algorithm
module KNN where

import Classifier
import Data.List (group, minimumBy, transpose, sort, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
-- import qualified Data.Text as T

-- | Scaling all values between 0 and 1
scaling :: Classifier -- ^ classifier to be scaled
        -> Classifier -- ^ scaled classifier
scaling c = c { objects = scaledObjects }
    where scaledObjects :: [Object]
          scaledObjects =
              map (\o -> o { variables = zipWith minmaxScaling minmax (variables o)})
                $ objects c
          -- minmax being for each column of variables the tuple (minimum value, maximum value)
          minmax = map (\vs -> (minimum vs, maximum vs)) . transpose . map variables $ objects c

-- | Rescaling a value (min-max normalization)
minmaxScaling :: Fractional a => (a, a) -> a -> a
minmaxScaling (min, max) x = (x - min) / (max - min)

-- | Weighted Euclidean distance between 2 entities
euclideanDistance :: [Double] -- ^ list of ordered weighs
                  -> Object   -- ^ object 1
                  -> Object   -- ^ object 2
                  -> Double   -- ^ weighted euclidean distance between o1 and o2
euclideanDistance ws o1 o2 = sqrt . sum $ weightedDists
    where weightedDists = zipWith (*) ws distances
          distances = zipWith (\v1 v2 -> (v2 - v1) ^ 2) (variables o1) (variables o2)

-- | List the nearest neighbours
nearestNeighbours :: Int                      -- ^ how many neighbours (the k in k-NN)
                  -> Object                   -- ^ the object for which we want to find the k-NN
                  -> Classifier               -- ^ the scaled classifier to be used
                  -> [(Text, Double, Double)] -- ^ the k nearest neighbours sorted by distance ASC
nearestNeighbours k e c = map (\(n, o, d) -> (n, fromJust . object $ o, d))
                        -- (name of the neighbour, class, euclidean distance)
                        . take k
                        . sortBy (\(_, _, d) (_, _, d') -> compare d d') -- sort by distance
                        . map (\x -> (name x, x, euclideanDistance (weighs c) e x)) -- euclidean distance with all entities
                        $ reliableObjects c

-- | Predict class for a non-classified object of a classifier
predictClass :: Int        -- ^ how many neighbours
             -> Object     -- ^ a non-classified object
             -> Classifier -- ^ the scaled classifier to be used
             -> Object     -- ^ the now classified object
predictClass k o c = o { object = Just predicted, neighbours = Just neighboursNames }
    where predicted = fst
                    . minimumBy (\(_, l) (_, l') -> compare l' l)
                    . map (\x -> (head x, length x))
                    . group
                    . sort
                    . map (\(_, c, _) -> c)
                    $ neighbours
          neighbours = nearestNeighbours k o c
          neighboursNames = map (\(n, _, _) -> n) neighbours

-- | Predict class for a complete classifier
predictClassifier :: Int        -- ^ how many neighbours
                  -> Classifier -- ^ scaled classifier with non-classified objects
                  -> Classifier -- ^ classifier with all its objects classified
predictClassifier k c = c { objects = objects' }
    where objects' = map classify (objects c)
          classify o
            | isJust . object $ o = o
            | otherwise = predictClass k o c
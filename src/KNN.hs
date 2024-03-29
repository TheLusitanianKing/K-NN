{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : KNN
-- Description : k-nn algorithm, scaling and distances
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module KNN where

import Input (Input(..), Object(..), reliableObjects)
import Data.List (sortBy, transpose)
import Data.Maybe (fromJust)
import Data.Text (Text)

-- | Scaling all values between 0 and 1
scaling :: Input -- ^ input to be scaled
        -> Input -- ^ scaled input
scaling i = i { _objects = scaledObjects }
    where scaledObjects :: [Object]
          scaledObjects =
              map (\o -> o { _features = zipWith minmaxScaling minmax (_features o) })
                $ _objects i
          -- minmax being for each column of variables the tuple (minimum value, maximum value)
          minmax = map (\vs -> (minimum vs, maximum vs)) . transpose . map _features $ _objects i

-- | Rescaling a value (min-max normalization)
minmaxScaling :: Fractional a => (a, a) -> a -> a
minmaxScaling (mini, maxi) x = (x - mini) / (maxi - mini)

-- | Weighted Euclidean distance between 2 entities
euclideanDistance :: [Double] -- ^ list of ordered weighs
                  -> Object   -- ^ object 1
                  -> Object   -- ^ object 2
                  -> Double   -- ^ weighted euclidean distance between o1 and o2
euclideanDistance ws o1 o2 = sqrt . sum $ weightedDists
    where weightedDists = zipWith (*) ws distances
          distances = zipWith (\v1 v2 -> (v2 - v1) ^ (2 :: Int)) (_features o1) (_features o2)

-- | List the nearest neighbours
nearestNeighbours :: Int                      -- ^ how many neighbours (the k in k-NN)
                  -> Object                   -- ^ the object for which we want to find the k-NN
                  -> Input                    -- ^ the scaled input to be used
                  -> [(Text, Double, Double)] -- ^ the k nearest neighbours sorted by distance ASC
nearestNeighbours k e i =
    map (\(n, o, d) -> (n, fromJust . _label $ o, d))
    -- (name of the neighbour, class, euclidean distance)
    . take k
    . sortBy (\(_, _, d) (_, _, d') -> compare d d') -- sort by distance
    . map (\x -> (_name x, x, euclideanDistance (_weighs i) e x)) -- euclidean distance with all entities
    $ reliableObjects i
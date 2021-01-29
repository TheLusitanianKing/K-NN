-- |
-- Module      : KNN
-- Description : k-nn algorithm + scaling, distances and so on.
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Prediction where

import Data.List (group, minimumBy, sort)
import Data.Maybe (isJust)
import Input (Input(..), Object(..))
import KNN (nearestNeighbours)

-- | Type to represent to the type of k-nn used
-- From a list of neighbours values, gives the class value
type Prediction = [Double] -> Double

-- | Prediction function for classification
classification :: Prediction
classification = fst
               . minimumBy (\(_, l) (_, l') -> compare l' l)
               . map (\x -> (head x, length x))
               . group
               . sort

-- | Prediction function for regression
regression :: Prediction
regression ds = sum ds / (fromIntegral . length $ ds)

-- | Predict class for a non-classified object of the input
predict :: Int        -- ^ how many neighbours
        -> Prediction -- ^ how the prediction is calculated
        -> Object     -- ^ a non-classified object
        -> Input      -- ^ the scaled input to be used
        -> Object     -- ^ the now classified object
predict k p o i
    | reliable o = error "Should not classify a reliable object."
    | otherwise = o { label = Just predicted, neighbours = Just neighboursNames }
    where predicted = p . map (\(_, c, _) -> c) $ neighbours
          neighbours = nearestNeighbours k o i
          neighboursNames = map (\(n, _, _) -> n) neighbours

-- | Predict class for the whole input
predictInput :: Int        -- ^ how many neighbours
             -> Prediction -- ^ the prediction style used
             -> Input      -- ^ scaled input with non-classified objects
             -> Input      -- ^ input with all its objects classified
predictInput k p i = i { objects = objects' }
    where objects' = map classify (objects i)
          classify o
            | isJust . label $ o = o
            | otherwise           = predict k p o i
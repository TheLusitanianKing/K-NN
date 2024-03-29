-- |
-- Module      : KNN
-- Description : k-nn predictions
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
    | _reliable o = error "Should not classify a reliable object."
    | otherwise  = o { _label = Just predicted, _neighbours = Just neighboursNames }
    where predicted       = p . map (\(_, c, _) -> c) $ ns
          ns              = nearestNeighbours k o i
          neighboursNames = map (\(n, _, _) -> n) ns

-- | Predict class for the whole input
predictInput :: Int        -- ^ how many neighbours
             -> Prediction -- ^ the prediction style used
             -> Input      -- ^ scaled input with non-classified objects
             -> Input      -- ^ input with all its objects classified
predictInput k p i = i { _objects = objects' }
    where objects' = map classify (_objects i)
          classify o
            | isJust . _label $ o = o
            | otherwise          = predict k p o i
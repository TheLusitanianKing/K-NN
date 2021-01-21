{-# LANGUAGE OverloadedStrings #-}

-- | Evaluation related operations
module Evaluation where

import Classifier
import Data.List (sortBy)

-- | Evaluating a classifier with an evaluation set
evaluating :: Classifier -- ^ classifier with everything predicted
           -> [Object]   -- ^ list of objects from the evaluation set
           -> Double     -- ^ score (from 0 to 1)
evaluating c = evaluating' (unreliableObjects c)

evaluating' :: [Object] -- ^ set of classified objects
            -> [Object] -- ^ set of evaluation objects
            -> Double   -- ^ score (from 0 to 1)
evaluating' os1 os2 = sum scores / total
    where sorted1 = sortBy objectSorting os1
          sorted2 = sortBy objectSorting os2
          total   = fromIntegral $ length scores
          objectSorting o1 o2 = compare (name o1) (name o2)
          scores = zipWith (\o o' -> if object o == object o' then 1 else 0) sorted1 sorted2
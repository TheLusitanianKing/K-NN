{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Evaluation
-- Description : Evaluating predictions
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Evaluation where

import Input
import Data.List (sortBy)

-- | Evaluating a classified input with an evaluation set
evaluating :: Input      -- ^ input with everything predicted
           -> [Object]   -- ^ list of objects from the evaluation set
           -> Double     -- ^ score (from 0 to 1)
evaluating = evaluating' . unreliableObjects

-- | Evaluating a list of objects
evaluating' :: [Object] -- ^ set of classified objects
            -> [Object] -- ^ set of evaluation objects
            -> Double   -- ^ score (from 0 to 1)
evaluating' os1 os2 = sum scores / total
    where sorted1 = sortBy objectSorting os1
          sorted2 = sortBy objectSorting os2
          total   = fromIntegral $ length scores
          objectSorting o1 o2 = compare (name o1) (name o2)
          scores = zipWith (\o o' -> if object o == object o' then 1 else 0) sorted1 sorted2
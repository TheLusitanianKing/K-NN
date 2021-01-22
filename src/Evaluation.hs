{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Evaluation
-- Description : Evaluating predictions
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Evaluation where

import Data.Maybe (fromJust)
import Input (Input(..), Object(..), Value, unreliableObjects)

-- | Evaluating a classified input with an evaluation set
evaluatingC :: Input      -- ^ input with everything predicted
            -> [Object]   -- ^ list of objects from the evaluation set
            -> Value      -- ^ score (from 0 to 1)
evaluatingC = evaluating . unreliableObjects
    where evaluating os1 os2 = sum scores / total
            where total  = fromIntegral $ length scores
                  scores = zipWith (\o1 o2 -> if object o1 == object o2 then 1 else 0) os1 os2

-- | SAE (sum of absolute errors)
sae :: Input    -- ^ input with everything predicted
    -> [Object] -- ^ list of objects from the evaluation set
    -> Value    -- ^ score
sae i = sae' (unreliableObjects i)
    where sae' os1 os2 = sum errors
            where errors = zipWith (\o1 o2 -> abs $ (fromJust . object $ o2) - (fromJust . object $o1)) os1 os2

-- | SSE (sum of squared errors)
sse :: Input    -- ^ input with everything predicted
    -> [Object] -- ^ list of objects from the evaluation set
    -> Value    -- ^ score
sse i = sse' (unreliableObjects i)
    where sse' predictions evaluatingObjs = sum errors
            where errors = zipWith (\o1 o2 -> ((fromJust . object $ o2) - (fromJust . object $ o1)) ^ 2) predictions evaluatingObjs

-- | Calculating R-Square value
-- Value between 0 and 1, bigger value indicates a better fit between prediction and actual value.
rsquare :: Input    -- ^ input with everything predicted
        -> [Object] -- ^ list of objects from the evaluation set
        -> Value    -- ^ score (between 0 and 1)
rsquare i = rsquare' (unreliableObjects i)
    where rsquare' predictions evaluatingObjs = 1 - (sse i evaluatingObjs / sum sumMean)
            where sumMean = map (\o -> ((fromJust . object $ o) - mean) ^ 2) evaluatingObjs
                  mean = sum (map (fromJust . object) predictions) / (fromIntegral . length $ predictions)

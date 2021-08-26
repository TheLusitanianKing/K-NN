{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Evaluation
-- Description : Evaluating predictions for k-nn regression
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Evaluation.Regression where

import Data.Maybe (fromJust)
import Input (Input(..), Object(..), Value, unreliableObjects)

-- | SAE (sum of absolute errors)
sae :: Input    -- ^ input with everything predicted
    -> [Object] -- ^ list of objects from the evaluation set
    -> Value    -- ^ score
sae i = sae' (unreliableObjects i)
    where
        sae' os1 os2 = sum $ errors os1 os2
        errors = zipWith (\o1 o2 -> abs $ (fromJust . _label $ o2) - (fromJust . _label $o1))

-- | SSE (sum of squared errors)
sse :: Input    -- ^ input with everything predicted
    -> [Object] -- ^ list of objects from the evaluation set
    -> Value    -- ^ score
sse i = sse' (unreliableObjects i)
    where
        sse' predictions evaluatingObjs = sum $ errors predictions evaluatingObjs
        errors = zipWith (\o1 o2 -> ((fromJust . _label $ o2) - (fromJust . _label $ o1)) ^ (2 :: Int))

-- | Calculating R-Square value
-- Value between 0 and 1, bigger value indicates a better fit between prediction and actual value.
rsquare :: Input    -- ^ input with everything predicted
        -> [Object] -- ^ list of objects from the evaluation set
        -> Value    -- ^ score (between 0 and 1)
rsquare i = rsquare' (unreliableObjects i)
    where
        rsquare' predictions evaluatingObjs =
            1 - (sse i evaluatingObjs / sum (sumMean evaluatingObjs predictions))
        sumMean os ps = map (\o -> ((fromJust . _label $ o) - mean ps) ^ (2 :: Int)) os
        mean ps  = sum (map (fromJust . _label) ps) / (fromIntegral . length $ ps)
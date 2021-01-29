{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Evaluation
-- Description : Evaluating predictions for k-nn classification
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Evaluation.Classification where

import Input (Input(..), Object(..), Value, unreliableObjects)

-- | Evaluating a classified input with an evaluation set
score :: Input      -- ^ input with everything predicted
      -> [Object]   -- ^ list of objects from the evaluation set
      -> Value      -- ^ score (from 0 to 1)
score = evaluating . unreliableObjects
    where evaluating os1 os2 = sum scores / total
            where total  = fromIntegral $ length scores
                  scores = zipWith (\o1 o2 -> if label o1 == label o2 then 1 else 0) os1 os2
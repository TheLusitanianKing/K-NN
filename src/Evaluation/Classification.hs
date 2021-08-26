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
score = eval . unreliableObjects
    where
        eval os1 os2 = let scs = scores os1 os2 in sum scs / total scs
        total scs = fromIntegral . length $ scs
        scores = zipWith (\o1 o2 -> if _label o1 == _label o2 then 1 else 0)
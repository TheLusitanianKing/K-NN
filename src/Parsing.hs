{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parsing
-- Description : Handle parsing data
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Parsing where

import Input
import Data.Text (Text)
import qualified Data.Text as T

-- | Parsing a CSV file
parseCSVFile :: Text       -- ^ content of the CSV file
             -> Int        -- ^ index of the class/variable to predict
             -> Input      -- ^ parsed input
parseCSVFile content i = parse . map (map T.strip . T.splitOn ",") . T.lines $ content
    where parse :: [[Text]] -> Input
          parse ((_:columns):weighs:objects) =
              if i >= length columns
              then error "Could not find the class/variable to predict in the CSV."
              else
                  Input {
                      -- to make it easier, we suppose the class will always have a - indicating a missing value
                      weighs  = map (read . T.unpack) (filter (/="-") weighs),
                      objects = map (parseObject i) objects
                  }
          parse _ = error "Missing data in the parsed CSV."

-- | Parsing a single object
parseObject :: Int    -- the index where the class is
            -> [Text] -- the list of variables
            -> Object -- the constructed object
parseObject i (name:values) =
    let v  = values !! i                               -- retrieving class value
        vs = let (a, _:b) = splitAt i values in a ++ b -- removing class from the variables
    in Object {
        name       = T.strip name,
        variables  = map (read . T.unpack) vs,
        object     = if v == "-" then Nothing else return $ (read . T.unpack) v,
        reliable   = v /= "-",
        neighbours = Nothing
    }
parseObject _ _ = error "Malformed entity value..."

-- | Parsing an evaluating set
parseEvaluatingCSVFile :: Text -> Int -> [Object]
parseEvaluatingCSVFile content i = map (parseObject i . map T.strip . T.splitOn ",") . T.lines $ content
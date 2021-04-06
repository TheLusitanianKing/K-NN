{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Input
-- Description : Handle input data and some helper functions
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Input where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Type alias for the values
type Value = Double

-- | Algebraic data type representing an object
data Object = Object {
    name       :: Text,        -- ^ its name
    features   :: [Value],     -- ^ its features (unscaled)
    label      :: Maybe Value, -- ^ its label (what we are predicting)
    reliable   :: Bool,        -- ^ if its class/variable is 100% reliable or if it has been predicted by k-NN
    neighbours :: Maybe [Text] -- ^ its neighbours' names for illustration purposes
} deriving (Eq, Show)

-- | Algebraic data type representing the input data
data Input = Input {
    weighs  :: [Value], -- ^ the weigh for each of the variables
    objects :: [Object] -- ^ the list of objects/data
} deriving (Eq)

-- | Custom print for the input
instance Show Input where
    show i = "Classified data: "
           ++ "\n"
           ++ concatMap
                (\o -> indent 4 ++ T.unpack (name o) ++ " -> " ++ maybe "X" show (label o) ++ "\n")
                (unreliableObjects i)

-- | Helper function to give an simple blank space for indentation purpose
indent :: Int -> String
indent x = concat (replicate x " ")

-- | Retrieving labeled and unlabeled objects of the input
-- And reliable/unreliable objects (whether or not it was classified by k-NN)
labeled, unlabeled, reliableObjects, unreliableObjects :: Input -> [Object]
labeled           = filter (isJust . label) . objects
unlabeled         = filter (isNothing . label) . objects
reliableObjects   = filter reliable . labeled
unreliableObjects = filter (not . reliable) . labeled
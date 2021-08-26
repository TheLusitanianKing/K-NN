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
data Object = Object
    { _name       :: Text         -- ^ its name
    , _features   :: [Value]      -- ^ its features (unscaled)
    , _label      :: Maybe Value  -- ^ its label (what we are predicting)
    , _reliable   :: Bool         -- ^ if its class/variable is 100% reliable or if it has been predicted by k-NN
    , _neighbours :: Maybe [Text] -- ^ its neighbours' names for illustration purposes
    } deriving (Eq, Show)

-- | Algebraic data type representing the input data
data Input = Input
    { _weighs  :: [Value]  -- ^ the weigh for each of the variables
    , _objects :: [Object] -- ^ the list of objects/data
    } deriving (Eq)

-- | Custom print for the input
instance Show Input where
    show i =
        "Classified data: "  ++ "\n"
        ++ concatMap
            (\o -> indent 4 ++ T.unpack (_name o) ++ " -> " ++ maybe "X" show (_label o) ++ "\n")
            (unreliableObjects i)

-- | Helper function to give an simple blank space for indentation purpose
indent :: Int -> String
indent x = concat (replicate x " ")

-- | Retrieving labeled and unlabeled objects of the input
-- And reliable/unreliable objects (whether or not it was classified by k-NN)
labeled, unlabeled, reliableObjects, unreliableObjects :: Input -> [Object]
labeled           = filter (isJust . _label) . _objects
unlabeled         = filter (isNothing . _label) . _objects
reliableObjects   = filter _reliable . labeled
unreliableObjects = filter (not . _reliable) . labeled
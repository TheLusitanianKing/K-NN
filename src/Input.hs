{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Input
-- Description : Handle input data and some helper functions
-- License     : MIT
-- Maintainer  : The Lusitanian King <alexlusitanian@gmail.com>
module Input where

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Type alias for the values
type Value = Double

-- | Algebraic data type representing an object
data Object = Object {
    name       :: Text,        -- ^ its name
    variables  :: [Value],     -- ^ its variable (unscaled)
    object     :: Maybe Value, -- ^ its class/variable to predict (if exists, already classified)
    reliable   :: Bool,        -- ^ if its class/variable is 100% reliable or if it has been predicted by k-NN
    neighbours :: Maybe [Text] -- ^ its neighbours' names for illustration purposes
} deriving (Show)

-- | Algebraic data type representing the input data
data Input = Input {
    weighs  :: [Value], -- ^ the weigh for each of the variables
    objects :: [Object] -- ^ the list of objects/data
}

-- | Custom print for the input
instance Show Input where
    show i = "Classified data: "
           ++ "\n"
           ++ concatMap
                (\o -> indent 4 ++ T.unpack (name o) ++ " -> " ++ maybe "X" show (object o) ++ "\n")
                (unreliableObjects i)

-- | Helper function to give an simple blank space for indentation purpose
indent :: Int -> String
indent x = concat (replicate x " ")

-- | Retrieving classified and unclassified objects of the input
-- And reliable/unreliable objects (whether or not it was classified by k-NN)
classified, unclassified, reliableObjects, unreliableObjects :: Input -> [Object]
classified = filter (isJust . object) . objects
unclassified = filter (isNothing . object) . objects
reliableObjects = filter reliable . classified
unreliableObjects = filter (not . reliable) . classified
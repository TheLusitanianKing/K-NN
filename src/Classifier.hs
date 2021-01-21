{-# LANGUAGE OverloadedStrings #-}

module Classifier where

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

-- | Algebraic data type representing the classifier (based on the input file)
data Classifier = Classifier {
    weighs  :: [Value],
    objects :: [Object]
}

-- | Custom print for the classifier
instance Show Classifier where
    show c = "Weighs: " ++ show (weighs c) ++ "\n"
           ++ "Reliable data: " ++ (show . length . reliableObjects $ c) ++ "\n"
           ++ "Classified data: " ++ "\n"
           ++ concatMap
                (\o ->
                    indent 4 ++ T.unpack (name o) ++ " -> " ++ maybe "X" show (object o) ++ "\n"
                    ++ indent 8 ++ (T.unpack . T.intercalate ", " . fromJust $ neighbours o) ++ "\n"
                )
                (unreliableObjects c)

-- | Helper function to give an simple blank space for indentation purpose
indent :: Int -> String
indent x = concat (replicate x " ")

-- | Retrieving classified and unclassified objects of the classifier
-- And reliable/unreliable objects (those objects are classified but determine or not by the k-NN)
classified, unclassified, reliableObjects, unreliableObjects :: Classifier -> [Object]
classified = filter (isJust . object) . objects
unclassified = filter (isNothing . object) . objects
reliableObjects = filter reliable . classified
unreliableObjects = filter (not . reliable) . classified
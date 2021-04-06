{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Evaluation.Classification (score)
import Evaluation.Regression (rsquare)
import KNN (scaling)
import Parsing (parseCSVFile, parseEvaluatingCSVFile)
import Prediction (classification, predictInput, regression)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- | Default k for k-nn algorithm (will be used when no k is explicitely passed)
defaultK :: Int
defaultK = 5

main :: IO ()
main = do
    args <- getArgs
    -- retrieving k in k-nn (will use default k if none is explicitely passed)
    let k = (if length args == 1 then fromMaybe defaultK $ readMaybe (head args) else defaultK)
    -- retrieve CSV files
    learningCSV            <- T.IO.readFile "data/football/learning.csv"
    classificationInputCSV <- T.IO.readFile "data/football/classification/input.csv"
    regressionInputCSV     <- T.IO.readFile "data/football/regression/input-stadium.csv"
    regressionInputCSV'    <- T.IO.readFile "data/football/regression/input-european.csv"
    -- k-nn classification
    knnClassification k 5 $ learningCSV `T.append` "\n" `T.append` classificationInputCSV
    putStrLn []
    -- k-nn regression
    knnRegression k 0 $ learningCSV `T.append` "\n" `T.append` regressionInputCSV -- stadium capacity
    putStrLn []
    knnRegression k 1 $ learningCSV `T.append` "\n" `T.append` regressionInputCSV' -- european trophies

knnClassification :: Int -> Int -> Text -> IO ()
knnClassification k index s = do
    -- parsing scaling and then predicting
    let scaled = scaling $ parseCSVFile s index
    let predicted = predictInput k classification scaled
    -- preparing evaluation data
    evaluationCSV <- T.IO.readFile "data/football/evaluation.csv"
    let evaluationData = parseEvaluatingCSVFile evaluationCSV index
    -- printing feedback
    putStrLn $ "Classification results: \n" ++ show predicted
    putStrLn $ "Evaluation (from 0 to 1): " ++ show (score predicted evaluationData)

knnRegression :: Int -> Int -> Text -> IO ()
knnRegression k index s = do
    -- parsing scaling and then predicting
    let scaled = scaling $ parseCSVFile s index
    let predicted = predictInput k regression scaled
    -- preparing evaluation data
    evaluationCSV <- T.IO.readFile "data/football/evaluation.csv"
    let evaluationData = parseEvaluatingCSVFile evaluationCSV index
    -- printing feedback
    putStrLn $ "Regression results: \n" ++ show predicted
    putStrLn $ "R-Square (between 0 and 1): " ++ show (rsquare predicted evaluationData)
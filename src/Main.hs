{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Evaluation (evaluating)
import KNN (scaling)
import Parsing (parseCSVFile, parseEvaluatingCSVFile)
import Prediction (classification, predictInput, regression)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    -- retrieving k in k-NN
    let defaultK = 5
    let k = (if length args == 1 then fromMaybe defaultK $ readMaybe (head args) else defaultK)
    -- retrieve CSV files
    learningCSV            <- T.IO.readFile "data/football/learning.csv"
    classificationInputCSV <- T.IO.readFile "data/football/classification/input.csv"
    regressionInputCSV     <- T.IO.readFile "data/football/regression/input.csv"
    -- k-nn classification
    knnClassification k $ learningCSV `T.append` "\n" `T.append` classificationInputCSV
    -- k-nn regression
    knnRegression k  $ learningCSV `T.append` "\n" `T.append` regressionInputCSV

knnClassification :: Int -> Text -> IO ()
knnClassification k s = do
    let index = 5
    -- parsing scaling and then predicting
    let scaled = scaling $ parseCSVFile s index
    let predicted = predictInput k classification scaled
    -- evaluating
    evaluationCSV <- T.IO.readFile "data/football/evaluation.csv"
    let evaluationData = parseEvaluatingCSVFile evaluationCSV index
    -- printing feedback
    putStrLn $ "Classification results: \n" ++ show predicted
    putStrLn . ("Evaluation (from 0 to 1): " ++) . show $ evaluating predicted evaluationData

knnRegression :: Int -> Text -> IO ()
knnRegression k s = do
    let index = 0
    -- parsing scaling and then predicting
    let scaled = scaling $ parseCSVFile s index
    let predicted = predictInput k regression scaled
    -- evaluating
    -- ... (TODO)
    -- printing feedback
    putStrLn $ "Regression results:" ++ show predicted
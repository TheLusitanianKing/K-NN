{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Evaluation (evaluating)
import KNN (predictClassifier, scaling)
import Parsing (parseCSVFile, parseEvaluatingCSVFile)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    -- retrieving k in k-NN
    let defaultK = 5
    let k = (if length args == 1 then fromMaybe defaultK $ readMaybe (head args) else defaultK)
    -- retrieve CSV files
    classifiedCSV   <- T.IO.readFile "data/europeanFootball/classified.csv"
    unclassifiedCSV <- T.IO.readFile "data/europeanFootball/classification/unclassified.csv"
    -- appending them
    let appendedFiles = classifiedCSV `T.append` "\n" `T.append` unclassifiedCSV
    -- parsing classifier
    let classifier = parseCSVFile appendedFiles "Playing in Europe"
    -- scaling
    let scaledClassifier = scaling classifier
    -- predicting classes
    let predictedClassifier = predictClassifier k scaledClassifier
    putStrLn . ("Predictions: " ++) . show $ predictedClassifier
    -- now evaluating the predictions
    evaluationCSV <- T.IO.readFile "data/europeanFootball/classification/evaluation.csv"
    -- TODO: pass column name again, instead of index
    let evaluationData = parseEvaluatingCSVFile evaluationCSV 5
    putStrLn . ("Evaluation (from 0 to 1): " ++) . show $ evaluating predictedClassifier evaluationData
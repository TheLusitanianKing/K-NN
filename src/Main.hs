{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import KNN

main :: IO ()
main = do
    classifiedCSV   <- T.IO.readFile "data/europeanFootball/classified.csv"
    unclassifiedCSV <- T.IO.readFile "data/europeanFootball/unclassified-pie.csv"
    let appendedFiles = classifiedCSV `T.append` "\n" `T.append` unclassifiedCSV
    let classifier = parseCSVFile appendedFiles "Playing in Europe"
    let scaledClassifier = scaling classifier
    print scaledClassifier
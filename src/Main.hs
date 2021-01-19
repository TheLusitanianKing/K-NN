module Main where

import Classifier

main :: IO ()
main = do
    cl <- parseCSVFile "data/europeanFootball.csv"
    case cl of
        Nothing -> putStrLn "Could not create a classifier from the CSV file..."
        Just c  -> do
            let scaledClassifier = scaling c
            -- TODO: maybe we should scaling when actually classifying so we don't lose any data
            -- It would also mean, we don't have to keep the min/max value
            print scaledClassifier
            -- print $ nearestNeighbours 5 () c
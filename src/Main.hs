module Main where

import LigaClassifier (nearestNeighbours, parseCSVFile)

main :: IO ()
main = do
    clubs <- parseCSVFile "data/liga-nos.csv"
    let benfica = head clubs
    mapM_ print $ nearestNeighbours 5 benfica (tail clubs)
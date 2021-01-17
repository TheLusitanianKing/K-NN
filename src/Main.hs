module Main where

import Classifier
import Liga (parseCSVFile)

main :: IO ()
main = parseCSVFile "data/liga-nos.csv" >>= mapM_ print
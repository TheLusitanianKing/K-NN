module Main where

import Classifier
import Liga

main :: IO ()
main = parseCSVFile "data/liga-nos.csv" >>= print

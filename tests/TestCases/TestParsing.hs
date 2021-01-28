{-# LANGUAGE OverloadedStrings #-}

module TestParsing where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Input
import Parsing
import Test.Hspec

input :: Input
input = Input {
    weighs = [1, 2, 3],
    objects = [
        Object {
            name = "Sport Lisboa e Benfica",
            variables = [303.1, 353, 934.29],
            object = Just 1,
            reliable = True,
            neighbours = Nothing
        },
        Object {
            name = "Futebol Clube do Porto",
            variables = [129.29, 104, 120],
            object = Just 0,
            reliable = True,
            neighbours = Nothing
        },
        Object {
            name = "Sporting Clube de Portugal",
            variables = [250.1, 120, 100],
            object = Nothing,
            reliable = False,
            neighbours = Nothing
        }]
}


(+-+) = T.append

rawCSV :: Text
rawCSV = "Name, A, B, C, D" +-+ "\n"
       +-+ "-, 1, 2, 3, -" +-+ "\n"
       +-+ "Sport Lisboa e Benfica, 303.1, 353, 934.29, 1" +-+ "\n"
       +-+ "Futebol Clube do Porto, 129.29, 104, 120, 0" +-+ "\n"
       +-+ "Sporting Clube de Portugal, 250.1, 120, 100, -"

parsingTests :: Spec
parsingTests = do
    describe "Testing input parsing..." $ do
        it "Parsing correct CSV file" $ do
            parseCSVFile rawCSV 3
            `shouldBe`
            input

        it "Parsing incorrect CSV file" $ do
            evaluate (parseCSVFile "" 0)
            `shouldThrow`
            anyErrorCall
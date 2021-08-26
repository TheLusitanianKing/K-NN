{-# LANGUAGE OverloadedStrings #-}

module TestParsing where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Input
import Parsing
import Test.Hspec

input :: Input
input = Input
    { _weighs = [1, 2, 3]
    , _objects =
        [ Object
            { _name       = "Sport Lisboa e Benfica"
            , _features   = [303.1, 353, 934.29]
            , _label      = Just 1
            , _reliable   = True
            , _neighbours = Nothing
            }
        , Object
            { _name       = "Futebol Clube do Porto"
            , _features   = [129.29, 104, 120]
            , _label      = Just 0
            , _reliable   = True
            , _neighbours = Nothing
            }
        , Object
            { _name       = "Sporting Clube de Portugal"
            , _features   = [250.1, 120, 100]
            , _label      = Nothing
            , _reliable   = False
            , _neighbours = Nothing
            }
        ]
    }

rawCSV :: Text
rawCSV = "Name, A, B, C, D" <> "\n"
       <> "-, 1, 2, 3, -" <> "\n"
       <> "Sport Lisboa e Benfica, 303.1, 353, 934.29, 1" <> "\n"
       <> "Futebol Clube do Porto, 129.29, 104, 120, 0" <> "\n"
       <> "Sporting Clube de Portugal, 250.1, 120, 100, -"

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
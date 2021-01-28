{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import TestClassificationEvaluation
import TestParsing
import TestRegressionEvaluation

main :: IO ()
main = hspec $ do
    parsingTests
    -- classificationEvaluationTests
    -- regressionEvaluationTests
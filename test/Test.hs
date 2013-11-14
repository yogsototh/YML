module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)
import YML.Dataset.Test
import YML.LinearGradient.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ datasetSuite
            , linearGradientSuite
            ]

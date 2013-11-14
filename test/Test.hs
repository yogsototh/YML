module Main where

import Test.Tasty (defaultMain,testGroup)
import YML.Dataset.Test
import YML.LinearGradient.Test

main :: IO ()
main = defaultMain $ testGroup "Tests"
            [ datasetSuite
            , linearGradientSuite
            ]

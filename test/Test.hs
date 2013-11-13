module Main where

import Test.Framework (defaultMain)
import YML.Dataset.Test
import YML.LinearGradient.Test

main :: IO ()
main = defaultMain [datasetSuite,linearGradientSuite]

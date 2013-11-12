module Main where

import Test.Framework (defaultMain)
import YML.Test

main :: IO ()
main = defaultMain [hmlSuite]

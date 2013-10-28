module Main where

import Test.Framework (defaultMain)
import Hml.Test

main :: IO ()
main = defaultMain [hmlSuite]

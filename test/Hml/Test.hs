module Hml.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Test.Framework.Providers.QuickCheck2 (testProperty)

import Hml

hmlSuite :: Test
hmlSuite = testGroup "Hml testing"
    [ testCase "Linear Cost Function" (testCost linearDataset 100)
    , testProperty "Cost Function QuickCheck" prop_cost]

linearDataset = [10,20,30,40]

testCost :: Dataset -> Integer -> Assertion
testCost ds expectedValue = expectedValue @=? cost ds

prop_cost :: [Integer] -> Bool
prop_cost list = sum list == cost list

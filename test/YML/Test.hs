module YML.Test where

import Test.Tasty (testGroup, Test)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import YML.Dataset
import YML.LinearGradient

ymlSuite :: Test
ymlSuite = testGroup "YML testing"
    [ testCase "Linear Cost Function" (testCost linearDataset 100)
    , SC.testProperty "Cost Function is positive" prop_cost_is_positive]

linearDataset = [10,20,30,40]

testCost :: Dataset -> Integer -> Assertion
testCost ds expectedValue = expectedValue @=? cost ds

prop_cost_is_positive :: Dataset -> Bool
prop_cost_is_positive = forAll $ \dataset -> cost dataset (nullF dataset) > 0

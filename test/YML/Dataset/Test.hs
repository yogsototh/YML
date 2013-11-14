module YML.Dataset.Test
    (datasetSuite)
where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit

import qualified    Data.Vector.Unboxed as V
import YML.Dataset

datasetSuite :: TestTree
datasetSuite = testGroup "Dataset"
    [testCase "parsing" testParseOneLine]

oneLine :: String
oneLine="5 10"

testParseOneLine :: Assertion
testParseOneLine = parse oneLine @=? Dataset [Value {xs = V.fromList [1,5], y = 10}]

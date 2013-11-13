module YML.Dataset.Test
    (datasetSuite)
where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified    Data.Vector.Unboxed as V
import YML.Dataset

datasetSuite :: Test
datasetSuite = testGroup "Dataset"
    [testCase "parsing" testParseOneLine]

oneLine="5 10"
testParseOneLine = parse oneLine @=? Dataset [Value {xs = V.fromList [1,5], y = 10}]

module YML.LinearGradient.Test
    (linearGradientSuite)
where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import YML.Dataset
import YML.LinearGradient

linearGradientSuite :: Test
linearGradientSuite = testGroup "LinearGradient"
    [testCase "cost from null function" testCost]

basicDataset = parse "0 0\n1 1\n2 2\n3 3"

testCost = (cost basicDataset (nullF basicDataset)) @=? 1.75

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module YML.LinearGradient.Test
    (linearGradientSuite)
where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import Data.Vector.Unboxed as V

import YML.Dataset
import YML.LinearGradient

-- Make instance of Dataset
-- we simply use consN Constr where N is the arity of Constr (SmallCheck)
-- we also needed the
-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.SmallCheck.Series
instance Monad m => Serial m Dataset        where series = cons1 Dataset
instance Monad m => Serial m Value          where series = cons2 Value
instance Monad m => Serial m (V.Vector R)   where series = cons1 V.fromList
-- Now we could test properties with smallcheck on Dataset type.

linearGradientSuite :: TestTree
linearGradientSuite = testGroup "LinearGradient"
    [ testCase "cost from null function" testCost
    , SC.testProperty "Cost is always positive" prop_cost_positive
    ]

basicDataset :: Dataset
basicDataset = parse "0 0\n1 1\n2 2\n3 3"

testCost :: Assertion
testCost = (cost basicDataset (nullF basicDataset)) @=? 1.75

prop_cost_positive :: Property IO
prop_cost_positive = changeDepth (+1) $ forAll $ \dataset -> (nbFeatures dataset < 1) || (cost dataset (nullF dataset) >= 0)

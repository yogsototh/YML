module YML.Dataset
(
   R
 , Value (..)
 , Dataset (..)
 , nbFeatures
 , parse
)
where

import              Data.List              (intercalate)
import              Data.Vector.Unboxed    ((!))
import qualified    Data.Vector.Unboxed as V

type R = Double -- Just in case I want to change the precision later

-- | Value
data Value = Value {xs :: V.Vector R, y :: R} -- by convention xs ! 0 = 1
    deriving (Eq)
instance Show Value where
    show (Value features value) = intercalate "," $
        map show (V.toList features) ++ [show value]

-- | Dataset
data Dataset = Dataset [Value] deriving (Eq)
instance Show Dataset where
    show (Dataset values) = "[" ++ intercalate "\n" (map show values) ++ "]"

-- | returns the number of features of a dataset
nbFeatures :: Dataset -> Int
nbFeatures (Dataset (v:_)) = V.length (xs v)
nbFeatures _ = 0

-- | Parse some string into a Dataset
-- Example:
--
-- > parse "10 20\n30 40"
parse :: String -> Dataset
parse fileContent = fillValues $ map words (lines fileContent)
    where
    -- Somehow parse file containing numbers into a dataset
    fillValues :: [[String]] -> Dataset
    fillValues l = Dataset (map fillOneValue l)
        where
            fillOneValue :: [String] -> Value
            fillOneValue line = Value (V.cons 1 (V.fromList (reverse rfeatures))) value
                where (value:rfeatures) = reverse $ map read line

module YML.Dataset
where

import              Data.List              (intercalate)
import              Data.Vector.Unboxed    ((!))
import qualified    Data.Vector.Unboxed as V

type R = Double -- Just in case I want to change the precision later

-- | Value
data Value = Value {xs :: V.Vector R, y :: R} -- by convention xs ! 0 = 1
instance Show Value where
    show (Value features value) = intercalate "," $
        map show (V.toList features) ++ [show value]

-- | Dataset
data Dataset = Dataset [Value]
instance Show Dataset where
    show (Dataset values) = "[" ++ intercalate "\n" (map show values) ++ "]"

-- | returns the number of features of a dataset
nbFeatures :: Dataset -> Int
nbFeatures (Dataset (v:_)) = V.length (xs v)
nbFeatures _ = error "Empty dataset"

-- | Parse some file into a Dataset
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
module YML.LinearGradient
(
  Parameters (..)
, LinearFunction
, nullF
, cost
, gradientDescent
)
where

import              Data.List              (intercalate)
import              Data.Vector.Unboxed    ((!))
import qualified    Data.Vector.Unboxed as V
import              YML.Dataset

-- swap comments to show debug traces
-- import              Debug.Trace            (trace)
trace :: a -> b -> b
trace _ x = x
--


data Parameters  = Parameters { alpha :: R , threshold :: R} deriving Show

-- | Linear Function type
data LinearFunction = LinearFunction {thetas :: V.Vector R}
instance Show LinearFunction where
    show l = intercalate ", " $ map (take 5 . show) $ V.toList (thetas l)

-- | The null function (use the dataset to determine the number of features)
nullF :: Dataset -> LinearFunction
nullF dataset = LinearFunction (V.fromList $ replicate k 0) -- h(x) = 0x + 0
    where
        k = nbFeatures dataset

-- | The hypothesis function (depends on theta)
h :: LinearFunction -> Value -> Double
h f v = V.foldl (\acc (x,t) -> acc + x*t) 0 (V.zip (xs v) (thetas f))

-- | The function giving the cost of some linear function relatively
-- to some dataset.
cost :: Dataset -> LinearFunction -> Double
cost (Dataset values) f = (sum (map ((**2).dist) values))/(2*m)
    where
     m = fromIntegral $ length values
     dist v = h f v - (y v)

type Variable = Int

cost' :: Variable -> Dataset -> LinearFunction -> Double
cost' i (Dataset values) f = (sum (map term values))/(2*m)
    where
     m = fromIntegral $ length values
     xi val = (xs val) ! i
     term v = (h f v - (y v)) * (xi v)


oneStepGradient :: Parameters -> Dataset -> LinearFunction -> LinearFunction
oneStepGradient opts dataset f = if bad f then
                                error "BAD f: Certainly alpha is too wide"
                            else
                                trace ((show f) ++ ": " ++ show (cost dataset f)) $ LinearFunction newthetas
    where
        bad phi = V.any (\x -> isNaN x || isInfinite x) (thetas phi)
        -- new_theta_j = theta_j - alpha * derive cost (theta0, theta1)
        newthetas = V.imap newcost (thetas f)
        newcost i x = x - (alpha opts) * cost' i dataset f

gradientDescent :: Parameters -> Dataset -> LinearFunction
gradientDescent opts t = snd $ head $ filter close $ zip gradients (tail gradients)
    where
        close :: (LinearFunction,LinearFunction) -> Bool
        close ((LinearFunction us),(LinearFunction vs)) = dist < threshold opts
            where
                dist = V.foldl (\acc (u,v) -> acc + (u-v)**2 ) 0 (V.zip us vs)
        gradients = iterate (oneStepGradient opts t) (nullF t)

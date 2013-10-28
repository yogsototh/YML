-- | The Hml module
module Hml
where

-- | A data set is mostly a list of data
type Dataset = [Integer]

-- | Cost function return the cost of some hypothesis relatively to some data
cost :: Dataset -> Integer
cost ds = sum ds

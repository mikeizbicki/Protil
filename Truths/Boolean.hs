module Truths.Boolean
    ( Boolean(T,F)
    , conjunction, disjunction
    , defaultTruthValue, truthOfInference
    , unity, disunity
    ) where

import Logic
-- import DataTypes

data Boolean = T | F deriving (Show, Eq)

instance TruthClass Boolean where
    
    conjunction T T = T
    conjunction T F = F
    conjunction F T = F
    conjunction F F = F
    
    disjunction T T = T
    disjunction T F = T
    disjunction F T = T
    disjunction F F = F
    
    unity = T
    disunity = F

instance TruthClass Double where
    
    conjunction x y = x*y
    
    disjunction x y = x + y - x*y
    
    unity = 1
    disunity = 0

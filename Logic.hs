module Logic
    ( TruthClass
    , conjunction, disjunction
    , defaultTruthValue, truthOfInference
    , unity, disunity
    , convert, convertDefaults
    , truthFetch, defaultTruth
    , TruthBox
    , conjunctionTB
    ) where

import Data.Dynamic
import Data.Typeable
-------------------------------------------------------------------------------
--
-- These headers must be redefined whenever you want to add a new logic

data TruthBox = NoTruth
              | TB1 Boolean
              | TB2 Double
    deriving (Eq,Show)

defaultTruth :: String
defaultTruth = "bool"

truthFetch :: String -> String -> TruthBox
truthFetch "bool"   x = TB1 $ convert x
truthFetch "double" x = TB2 $ convert x

conjunctionTB :: TruthBox -> TruthBox -> TruthBox
conjunctionTB (TB1 x) (TB1 y) = TB1 (conjunction x y)
conjunctionTB (TB2 x) (TB2 y) = TB2 (conjunction x y)

-- NOTE:
--
-- Due to current Haskell standards, we cannot derive existentially quantified classes,
-- therefore we must do the extra typing to register a new controller and cannot let
-- Haskell do it for us as in this example:
-- 
--     data TruthBoxEx = forall a. (TruthClass a) => TruthBoxEx a
--         deriving (Show, Eq)
-- 
--     truthFetch :: String -> String -> TruthBoxEx
--     truthFetch "bool"   x = TruthBoxEx $ convert (x :: Boolean)
--     truthFetch "double" x = TruthBoxEx $ convert (x :: Double)
-- 
--     conjunctionTB :: TruthBoxEx -> TruthBoxEx -> TruthBoxEx
--     conjunctionTB x y {- same type -} = TB1 (conjunction x y)
--     conjunctionTB x y {- diff type -} = error "different types"
-- 
-- See http://hackage.haskell.org/trac/haskell-prime/wiki/ExistentialQuantification
--

class (Eq a, Show a) => TruthClass a where

    -- these must be implemented
    conjunction :: a -> a -> a
    disjunction :: a -> a -> a
    
    unity :: a
    disunity :: a
    
    convert :: String -> a 

    -- these defaults may be overridden to change operation
    defaultTruthValue :: a
    defaultTruthValue = unity
    
    truthOfInference :: a
    truthOfInference = unity
        
    convertDefaults :: String -> a
    convertDefaults "unity" = unity
    convertDefaults "disunity" = disunity
    convertDefaults "defaultTruthValue" = unity
    convertDefaults "truthOfInference" = unity
    
    
-------------------------------------------------------------------------------
-- TruthController Boolean
    
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
    
    convert "T" = T
    convert "F" = F
    convert x = convertDefaults x

-------------------------------------------------------------------------------
-- Percentage Logic

instance TruthClass Double where
    
    conjunction x y = x*y
    disjunction x y = x + y - x*y
    
    unity = 1
    disunity = 0
    
    convert x = convertDefaults x
    
import Control.Monad
import Data.Either
import Data.Dynamic
import Control.Monad.Instances

class (Show a, Eq a) => TruthClass a where
    conjunction :: a -> a -> a
    disjunction :: a -> a -> a
    
instance TruthClass Bool where
    
    conjunction True  True  = True
    conjunction True  False = False
    conjunction False True  = False
    conjunction False False = False
    
    disjunction True  True  = True
    disjunction True  False = True
    disjunction False True  = True
    disjunction False False = False
    
instance TruthClass Double where
    conjunction x y = x*y
    disjunction x y = x + (1-x)*y
    
{-
classReturn :: (TruthClass a) => String -> a
classReturn "True" = True
classReturn "False" = False
classReturn "1" = 1
classReturn "0" = 0
    
conjunction (classReturn "True") (classReturn "1")
-}

classReturn :: String -> Dynamic
classReturn "True" = toDyn True
classReturn "False" = toDyn False
classReturn "1" = toDyn (1 :: Double)
classReturn "0" = toDyn (0 :: Double)

-- success = conjunction t f
    
t = fromDyn (classReturn "1") $ Nothing
f = fromDyn (classReturn "False") $ Nothing
    
-- funky :: (TruthClass a) => String -> a
-- funky "True" = True
-- funky "False" = False
-- funky "1" = 1
-- funky "0" = 0
    
-- data (TruthClass a) => TruthObject a = 
--     TruthObject { unity :: a
--                 , disunity :: a
--                 }
--     deriving (Show,Eq)
--        
-- truthBoolean = TruthObject 
--     { unity = True
--     , disunity = False
--     }
--     
-- truthDouble = TruthObject 
--     { unity = 1.0 :: Double
--     , disunity = 0.0 :: Double
--     }
    
    
-- funky :: String -> Bool
-- funky "True" = True
-- funky "False" = False
-- 
-- funky :: String -> Double
-- funky "1" = 1
-- funky "0" = 0

-- data Selector = S1 Bool 
--               | S2 Double
-- 
-- -- instance Monad (Selector) where
-- --     return = S2
-- --     S1 b >>= f = f b
-- --     S2 c >>= f = f c
--     
-- monkey = do
--     x <- funky "1"
--     y <- funky "0"
--     return $ conjunction x y
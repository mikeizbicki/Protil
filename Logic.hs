module Logic
    ( TruthClass
    , conjunction, disjunction
    , defaultTruthValue, truthOfInference
    , unity, disunity
    , convert, convertDefaults
    , truthFetch, parseTruthBox
    , defaultTruth
    , TruthBox
    , conjunctionTB
    , enableTraceBox
    ) where

-- import Data.Dynamic
-- import Data.Typeable
import Text.ParserCombinators.Parsec

-------------------------------------------------------------------------------
--
-- These headers must be redefined whenever you want to add a new logic

data TruthBox = TB1 Boolean
              | TB2 Double
              | TB3 MaybeLogic
    deriving (Eq)

instance Show TruthBox where
    show (TB1 x) = show x
    show (TB2 x) = show x
    show (TB3 x) = show x

defaultTruth :: String
defaultTruth = "bool"

truthFetch :: String -> String -> TruthBox
truthFetch "bool"           x = TB1 $ convert x
truthFetch "fuzzy"          x = TB2 $ convert x
truthFetch "maybe"          x = TB3 $ convert x

conjunctionTB :: TruthBox -> TruthBox -> TruthBox
conjunctionTB (TB1 x) (TB1 y) = TB1 (conjunction x y)
conjunctionTB (TB2 x) (TB2 y) = TB2 (conjunction x y)
conjunctionTB (TB3 x) (TB3 y) = TB3 (conjunction x y)
conjunctionTB x y = error $ show x ++ " - " ++show y

enableTraceBox :: TruthBox -> Bool
enableTraceBox (TB1 x) = enableTrace x
enableTraceBox (TB2 x) = enableTrace x
enableTraceBox (TB3 x) = enableTrace x

parseTruthBox :: Parser TruthBox
parseTruthBox =  parseTB1
             <|> parseTB2
             <|> parseTB3

parseTB1 = parseT <|> parseF
    where parseT = char 'T' >> (return $ TB1 T)
          parseF = char 'F' >> (return $ TB1 F)
          
parseTB2 = do
    str1 <- digit
    str2 <- option "" (char '.' >> many digit)
    let dbl = read (str1:'.':str2)
    return $ TB2 dbl
        
parseTB3 = parseT <|> parseF <|> parseM
    where parseT = char 'T' >> (return $ TB3 Tm)
          parseF = char 'F' >> (return $ TB3 Fm)
          parseM = char 'M' >> (return $ TB3 Mm)
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
    
--     parseValue :: Parser a

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
    
    enableTrace :: a -> Bool
    enableTrace _ = False
    
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
    
--     parseValue = parseT <|> parseF
--         where parseT = char 'T' >> return T
--               parseF = char 'F' >> return F
    
    convert x = convertDefaults x

-------------------------------------------------------------------------------
-- Fuzzy Logic

instance TruthClass Double where
    
    conjunction x y = x*y
    disjunction x y = x + y - x*y
    
    unity = 1
    disunity = 0

--     parseValue = do
--         str <- many (digit <|> char '.')
--         let dbl = read str
--         return dbl

    convert x = convertDefaults x

-------------------------------------------------------------------------------
-- Maybe Logic

data MaybeLogic = Tm | Fm | Mm 
    deriving (Eq)

instance Show MaybeLogic where
    show Tm = "True"
    show Fm = "False"
    show Mm = "Maybe"

instance TruthClass MaybeLogic where
    
    conjunction Tm Tm = Tm
    conjunction Tm Fm = Fm
    conjunction Tm Mm = Mm
    conjunction Fm Tm = Fm
    conjunction Fm Fm = Fm
    conjunction Fm Mm = Fm
    conjunction Mm Tm = Mm
    conjunction Mm Fm = Fm
    conjunction Mm Mm = Mm
    
    disjunction Tm Tm = Tm
    disjunction Tm Fm = Tm
    disjunction Tm Mm = Tm
    disjunction Fm Tm = Tm
    disjunction Fm Fm = Fm
    disjunction Fm Mm = Mm
    disjunction Mm Tm = Tm
    disjunction Mm Fm = Mm
    disjunction Mm Mm = Mm
    
    unity = Tm
    disunity = Fm
    
--     parseValue = parseT <|> parseF
--         where parseT = char 'T' >> return T
--               parseF = char 'F' >> return F
    
    convert x = convertDefaults x

    enableTrace Mm = True
    enableTrace _  = False
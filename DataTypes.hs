module DataTypes
    ( Term(Atom,Var,CTerm), functor, args
    , Rule(Rule), ruleL, ruleR, ruleTruth
    , RulesDB(RulesDB), dbTruthController, dbRules
    , Binding, Bindings
    , TruthList(TruthList), truthVal, truthList
    , truthListAppend
    
    , truthFetch, truthFetch'
    , parseTruthBox
    , defaultTruth
    , TruthBox(LogicTracer)
    ) where

import Data.Monoid
import Text.ParserCombinators.Parsec

import Logic

-- This file contains all the datatypes that are not directly related to implementing
-- specific truth controllers

data Term = Atom String
          | Var String
          | CTerm { functor :: String, args :: [Term] }
    deriving (Eq)
    
instance Show Term where
    show (Atom s) = s
    show (Var s)  = s
    show (CTerm f a) = f ++ "(" ++ ppShowTerms a ++ ")"

ppShowTerms :: [Term] -> String
ppShowTerms [x]    = show x
ppShowTerms (x:xs) = show x ++ "," ++ ppShowTerms xs
ppShowTerms y      = "???" ++ show y ++ "???"

data RulesDB = RulesDB { dbTruthController :: String, dbRules :: [Rule] }
data Rule = Rule { ruleL :: Term, ruleR :: [Term], ruleTruth :: TruthBox }
    deriving (Eq)
    
instance Show Rule where
    show (Rule l r t) = show l ++ body ++ truth ++"."
        where body = if length r == 0
                        then ""
                        else ":-" ++ ppShowTerms r
              truth = "<-" ++ show t

type Binding = (Term,Term)
type Bindings = TruthList -- Binding

data TruthList = TruthList { truthVal :: TruthBox, truthList :: [Binding]}
    deriving (Eq)

instance Show TruthList where
-- instance (Show a) => Show (TruthList a) where
    show (TruthList truth list) = show truth ++ ": " ++ show list

-- instance Functor TruthList where
--     fmap f (TruthList truth list) = TruthList truth $ map f list

-- instance (TruthClass a) => Monoid (TruthList a b) where
--     mempty = TruthList defaultTruthValue []
--     mappend (TruthList t1 xs1) (TruthList t2 xs2) = TruthList (conjunction t1 t2) (xs1 ++ xs2)

truthListAppend :: TruthList -> TruthList -> TruthList
truthListAppend tl1@(TruthList t1 xs1) tl2@(TruthList t2 xs2) = TruthList (conjunctionTB t1 t2) (xs1 ++ xs2)

-------------------------------------------------------------------------------
--
-- These headers must be redefined whenever you want to add a new logic

data TruthBox = NoTruth
              | LogicTracer TruthBox [Rule]
              | TB1 Boolean
              | TB2 Double
              | TB3 MaybeLogic
    deriving (Eq,Show)

defaultTruth :: String
defaultTruth = "bool"

truthFetch :: String -> String -> TruthBox
truthFetch x y = LogicTracer (truthFetch' x y) []

truthFetch' :: String -> String -> TruthBox
truthFetch' "bool"           x = TB1 $ convert x
truthFetch' "fuzzy"          x = TB2 $ convert x
truthFetch' "maybe"          x = TB3 $ convert x
                          
conjunctionTB :: TruthBox -> TruthBox -> TruthBox
conjunctionTB (TB1 x) (TB1 y) = TB1 (conjunction x y)
conjunctionTB (TB2 x) (TB2 y) = TB2 (conjunction x y)
conjunctionTB (TB3 x) (TB3 y) = TB3 (conjunction x y)
conjunctionTB (LogicTracer tb1 l1) (LogicTracer tb2 l2) = LogicTracer (conjunctionTB tb1 tb2) (left++right)
    where left = if enableTraceBox tb1
                    then l1
                    else []
          right = if enableTraceBox tb2
                     then l2
                     else []
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

data MaybeLogic = Tm | Fm | Mm deriving (Show, Eq)

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
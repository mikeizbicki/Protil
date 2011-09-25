module DataTypes
    ( Term(Atom,Var,CTerm), functor, args
    
    , Rule(Rule), ruleL, ruleR, ruleTruth
    , RulesDB(RulesDB), dbTruthController, dbRules
    , emptyRulesDB
    
    , Binding
    , TruthList(TruthList), truthVal, truthList
    , truthListAppend
    
    , TracerBox (TracerBox)
    , tracerFetch
    ) where

import Data.Monoid
import Text.ParserCombinators.Parsec

import Logic

-- This file contains all the datatypes that are not directly related to implementing
-- specific truth controllers


---------------------------------------
-- Term

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

---------------------------------------
-- Rules

data RulesDB = RulesDB { dbTruthController :: String, dbRules :: [Rule] }
emptyRulesDB = RulesDB "" []
    
data Rule = Rule { ruleL :: Term, ruleR :: [Term], ruleTruth :: TracerBox }
    deriving (Eq)
    
instance Show Rule where
    show (Rule l r t) = show l ++ body ++ truth -- ++"."
        where body = if length r == 0
                        then ""
                        else ":-" ++ ppShowTerms r
              truth = "<-" ++ show t

---------------------------------------
-- TruthList

type Binding = (Term,Term)

data TruthList = TruthList { truthVal :: TracerBox, truthList :: [Binding]}
    deriving (Eq)

instance Show TruthList where
    show (TruthList truth list) = show truth ++ ": " ++ show list

truthListAppend :: TruthList -> TruthList -> TruthList
truthListAppend tl1@(TruthList t1 xs1) tl2@(TruthList t2 xs2) = TruthList (conjunctionTracer t1 t2) (xs1 ++ xs2)

---------------------------------------
-- TracerBox

data TracerBox = TracerBox TruthBox [Rule]
    deriving (Eq)

instance Show TracerBox where
    show (TracerBox tb r) = show tb ++ ruleStr
        where 
              ruleStr = if length r == 0
                           then ""
                           else show r

tracerFetch :: String -> String -> TracerBox
tracerFetch x y = TracerBox (truthFetch x y) []

conjunctionTracer :: TracerBox -> TracerBox -> TracerBox
conjunctionTracer (TracerBox tb1 l1) (TracerBox tb2 l2) = TracerBox (conjunctionTB tb1 tb2) (left++right)
    where left = if enableTraceBox tb1
                    then l1
                    else []
          right = if enableTraceBox tb2
                     then l2
                     else []
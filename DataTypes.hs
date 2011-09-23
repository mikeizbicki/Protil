module DataTypes
    ( Term(Atom,Var,CTerm), functor, args, Terms
    , Rule(Rule), ruleL, ruleR, ruleTruth
    , Rules
    , RulesDB(RulesDB), dbTruthController, dbRules
    , Binding, Bindings
    , TruthList(TruthList), truthVal, truthList
    , truthListAppend
    ) where

import Data.Monoid

import Logic

-- This file contains all the datatypes that are not directly related to implementing
-- specific truth controllers

type Terms = [Term]
data Term = Atom String
          | Var String
          | CTerm { functor :: String, args :: [Term] }
    deriving (Show,Eq)

data RulesDB = RulesDB { dbTruthController :: String, dbRules :: [Rule] }
type Rules = [Rule]
data Rule = Rule { ruleL :: Term, ruleR :: [Term], ruleTruth :: TruthBox }
    deriving (Show,Eq)

type Binding = (Term,Term)
type Bindings = TruthList Binding

data TruthList a = TruthList { truthVal :: TruthBox, truthList :: [a]}
    deriving (Eq)

instance (Show a) => Show (TruthList a) where
    show (TruthList truth list) = show truth ++ ": " ++ show list

instance Functor TruthList where
    fmap f (TruthList truth list) = TruthList truth $ map f list

-- instance (TruthClass a) => Monoid (TruthList a b) where
--     mempty = TruthList defaultTruthValue []
--     mappend (TruthList t1 xs1) (TruthList t2 xs2) = TruthList (conjunction t1 t2) (xs1 ++ xs2)

truthListAppend :: TruthList a -> TruthList a -> TruthList a
truthListAppend (TruthList t1 xs1) (TruthList t2 xs2) = TruthList (conjunctionTB t1 t2) (xs1 ++ xs2)
module DataTypes
    ( Term(Atom,Var,CTerm), functor, args, Terms
    , Rule(Rule), ruleL, ruleR, ruleTruth, Rules
    , Binding, Bindings
    , TruthList(TruthList), truthVal, truthList
    , arity, hasVar, isBindingAuto
    , ppShowBindings
    , falseBindings
--     , Truths.Boolean
    ) where

import Data.Monoid
import Control.Monad

import Logic
import Truths.Boolean

-- data types

data Term = Atom String
          | Var String
          | CTerm { functor :: String, args :: [Term] }
    deriving (Show,Eq)

data (TruthClass a) => Rule a = Rule { ruleL :: Term, ruleR :: [Term], ruleTruth :: a }
    deriving (Show,Eq)

type Rules a = [Rule a]
type Terms = [Term]

type Binding = (Term,Term)
-- type Bindings = [Binding]
-- type Bindings = (TruthType, [Binding])

data (TruthClass a) => TruthList a b = TruthList { truthVal :: a, truthList :: [b]}
    deriving (Eq)

type Bindings a = TruthList a Binding

instance (TruthClass a) => Functor (TruthList a) where
    fmap f (TruthList truth list) = TruthList truth $ map f list

instance (TruthClass a) => Monoid (TruthList a b) where
    mempty = TruthList defaultTruthValue []
    mappend (TruthList t1 xs1) (TruthList t2 xs2) = TruthList (conjunction t1 t2) (xs1 ++ xs2)

-- instance Monad TruthList where
--     return x = TruthList defaultTruthValue [x]
--     fail x = TruthList disunity []
--     xs >>= f = mconcat (truthList $ fmap f xs ) -- FIXME: Does this even work?

instance (TruthClass a, Show a, Show b) => Show (TruthList a b) where
    show (TruthList truth list) = show truth ++ ": " ++ show list

-- helper funcs

arity :: Term -> Int
arity (Atom x)  = 0
arity (Var x)   = 0
arity x         = length $ args x

hasVar :: Term -> Bool
hasVar (Var _)  = True
hasVar (Atom _) = False
hasVar term     = elem True $ map hasVar (args term)

isBindingAuto :: Binding -> Bool
isBindingAuto (Var v, _) = '@' `elem` v
isBindingAuto _          = False

ppShowBinding :: Binding -> String
ppShowBinding (Var v, Atom a) = v ++ "=" ++ a

ppShowBindings :: (TruthClass a) => Bindings a -> String
ppShowBindings xs = concat $ truthList $ fmap ((\ x -> x ++ "\n" ) . ppShowBinding) xs
-- ppShowBindings (t,xs) = t ++ "\n" ++ (concat $ map ((\ x -> x ++ "\n" ) . ppShowBinding) xs)

falseBindings :: (TruthClass a) => TruthList a b
falseBindings = TruthList disunity []
module DataTypes
    ( Term(Atom,Var,CTerm), functor, args, Terms
    , Rule(Rule), ruleL, ruleR, Rules
    , Binding, Bindings
    , TruthList(TruthList), truthVal, truthList
    , TruthType
    , arity, hasVar, isBindingAuto
    , ppShowBindings
    , falseBindings
    ) where

import Data.Monoid
import Control.Monad

import LogicBoolean

-- data types

data Term = Atom String
          | Var String
          | CTerm { functor :: String, args :: [Term] }
    deriving (Show,Eq)

data Rule = Rule { ruleL :: Term, ruleR :: [Term], ruleTruth :: TruthType }
    deriving (Show,Eq)

type Rules = [Rule]
type Terms = [Term]

type Binding = (Term,Term)
-- type Bindings = [Binding]
-- type Bindings = (TruthType, [Binding])

data TruthList a = TruthList { truthVal :: TruthType, truthList :: [a]}
    deriving (Eq)

type Bindings = TruthList Binding

instance Functor TruthList where
    fmap f (TruthList truth list) = TruthList truth $ map f list

instance Monoid (TruthList a) where
    mempty = TruthList defaultTruthValue []
    mappend (TruthList t1 xs1) (TruthList t2 xs2) = TruthList (conjunction t1 t2) (xs1 ++ xs2)

-- instance Monad TruthList where
--     return x = TruthList defaultTruthValue [x]
--     fail x = TruthList disunity []
--     xs >>= f = mconcat (truthList $ fmap f xs ) -- FIXME: Does this even work?

instance (Show a) => Show (TruthList a) where
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

ppShowBindings :: Bindings -> String
ppShowBindings xs = concat $ truthList $ fmap ((\ x -> x ++ "\n" ) . ppShowBinding) xs
-- ppShowBindings (t,xs) = t ++ "\n" ++ (concat $ map ((\ x -> x ++ "\n" ) . ppShowBinding) xs)

falseBindings = TruthList disunity []
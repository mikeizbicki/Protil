module DataTypes
    ( Term(Atom,Var,CTerm), functor, args, Terms
    , Rule(Rule), ruleL, ruleR
    , Rules, ruleCons
    , Binding, Bindings
    , TruthList(TruthList), truthVal, truthList
    , arity, hasVar, isBindingAuto
    , ppShowBindings
    , falseBindings
    
    , TruthType
    , TruthController
    , conjunction, disjunction
    , defaultTruthValue, truthOfInference
    , unity, disunity
    , prolog
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

-- data Rules = Rules [Rule]

data RuleController a = RuleController [a]
-- instance Monad RuleController where
--     return x = RuleController [x]
--     fail x = TruthList disunity []
--     (RuleController xs) >>= f = concat (map f xs)

ruleCons :: Rule -> Rules -> Rules
-- ruleCons x (Rules xs) = Rules (x:xs)
ruleCons x (xs) = (x:xs)

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
    mempty = TruthList (defaultTruthValue prolog) []
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




data TruthController = TS
    { name :: String
    , unity :: TruthType
    , disunity :: TruthType
    , defaultTruthValue :: TruthType
    , truthOfInference :: TruthType
    , falseBindings :: TruthList Binding
    }

prolog = TS
    { name              = "prolog"
    , unity             = TT_Percent 1
    , disunity          = TT_Percent 0
    , defaultTruthValue = unity prolog
    , truthOfInference  = unity prolog
    , falseBindings     = TruthList (disunity prolog) []
    }
    
data TruthType = TT_Boolean String 
               | TT_Maybe String 
               | TT_Percent Double
    deriving (Eq, Show)
    
conjunction :: TruthType -> TruthType -> TruthType
conjunction (TT_Percent x) (TT_Percent y) = TT_Percent (x*y)

disjunction :: TruthType -> TruthType -> TruthType
disjunction (TT_Percent x) (TT_Percent y) = TT_Percent (x + (1-x)*y)

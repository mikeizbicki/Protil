module Core 
    ( prove
    ) where

import Data.Monoid

import DataTypes
import Logic
import Parser

-------------------------------------------------------------------------------
-- Main solving functions
-- FIXME: make this code actually readable

prove :: RulesDB -> [Term] -> [Bindings]
prove (RulesDB controllerStr rules) goals = extractValidBindings $ prove' controllerStr rules 1 goals
    where extractValidBindings xs = fmap (\x -> TruthList (truthVal x) $ filter (not . isBindingAuto) (truthList x)) $ filter (\x -> truthVal x /= (truthFetch controllerStr "disunity")) $ justs xs
--     where extractValidBindings xs = {-fmap (\x -> TruthList (truthVal x) $ filter (not . isBindingAuto) (truthList x)) $ filter (\x -> truthVal x /= (truthFetch controllerStr disunity)) $-} justs xs

prove' :: String -> Rules -> Int -> Terms -> [Maybe Bindings]
prove' controllerStr rules i goals = do
    rule' <- decorateRules i rules
    let (newBindings, newGoals) = branch controllerStr rule' goals
    let answer = if newGoals == []
                    then [newBindings]
                    else if maybeTruth newBindings == truthFetch controllerStr "disunity"
                            then [Nothing]
                            else maybeCons newBindings (prove' controllerStr rules (i+1) newGoals)
    answer
        where maybeTruth (Just (TruthList truth list)) = truth

branch :: String -> Rule -> Terms -> (Maybe Bindings, Terms)
branch controllerStr rule (goal:goals) = (newBindings, sub (unMaybe controllerStr newBindings) (newGoals++goals))
    where Rule nextTerm body truthValue = rule
          newGoals = body
          newBindings = disunityToMaybe $ unifyTerms controllerStr nextTerm goal

unifyTerms :: String -> Term -> Term -> Bindings
unifyTerms controllerStr (CTerm _ []) (CTerm _ []) = TruthList (truthFetch controllerStr "defaultTruthValue") []
unifyTerms controllerStr (CTerm _ []) (CTerm _ _)  = falseBindings controllerStr 
unifyTerms controllerStr (CTerm _ _ ) (CTerm _ []) = falseBindings controllerStr 
unifyTerms controllerStr (CTerm f1 (x:xs)) (CTerm f2 (y:ys))
    | f1 /= f2  = falseBindings controllerStr 
    | length xs /= length ys = falseBindings controllerStr 
    | hasVar x  = 
        let rest = unifyTerms controllerStr (subTerm (x,y) t1) (subTerm (x,y) t2) in
            (TruthList (truthFetch controllerStr "truthOfInference") [(x,y)]) `truthListAppend` rest
    | hasVar y  = 
        let rest = unifyTerms controllerStr (subTerm (y,x) t1) (subTerm (y,x) t2) in
            (TruthList (truthFetch controllerStr "truthOfInference") [(y,x)]) `truthListAppend` rest
    | x == y    = unifyTerms controllerStr t1 t2
    | otherwise = falseBindings controllerStr 
    where
          t1 = CTerm f1 xs
          t2 = CTerm f2 ys
unifyTerms controllerStr a1 a2 = error ( "Non-exhaustive blah: a1=" ++ (show a1) ++ " a2=" ++ (show a2))

---------------------------------------
-- Minor functions for unification

sub :: Bindings -> [Term] -> [Term]
sub (TruthList truth []) ts       = ts
sub (TruthList truth (b:bs)) ts   = sub (TruthList truth bs) $ subTerms b ts

subTerms :: Binding -> [Term] -> [Term]
subTerms b ts = map (subTerm b) ts

subTerm :: Binding -> Term -> Term
subTerm (var, atom) (Var v) 
    | var == (Var v)    = atom
    | otherwise         = Var v
subTerm _ (Atom a)                  = Atom a
subTerm binding (CTerm func args)   = CTerm func $ map (subTerm binding) args

decorateRules :: Int -> Rules -> Rules
decorateRules i rules = map (decorateRule i) rules

decorateRule :: Int -> Rule -> Rule
decorateRule i (Rule l rs tv) = Rule (decorateTerm i l) (map (decorateTerm i) rs) tv

decorateTerm :: Int -> Term -> Term
decorateTerm i (Atom x)             = Atom x
decorateTerm i (Var v)              = Var ("_"++(show i)++"@"++v)
decorateTerm i (CTerm func args)    = CTerm func $ map (decorateTerm i) args

hasVar :: Term -> Bool
hasVar (Var _)  = True
hasVar (Atom _) = False
hasVar term     = elem True $ map hasVar (args term)

falseBindings :: String -> TruthList a
falseBindings controlStr = TruthList (truthFetch controlStr "disunity") []

isBindingAuto :: Binding -> Bool
isBindingAuto (Var v, _) = '@' `elem` v
isBindingAuto _          = False

---------------------------------------
-- Pretty print

ppShowBinding :: Binding -> String
ppShowBinding (Var v, Atom a) = v ++ "=" ++ a

ppShowBindings :: Bindings -> String
ppShowBindings x = show x
-- ppShowBindings (TruthList t [])     = ""
-- ppShowBindings (TruthList t x:xs)   = show x ++ "\n" ++ (ppShowBindings 

   -- ppShowBindings xs = concat $ truthList $ fmap ((\ x -> x ++ "\n" ) . ppShowBinding) xs
-- ppShowBindings (t,xs) = t ++ "\n" ++ (concat $ map ((\ x -> x ++ "\n" ) . ppShowBinding) xs)

---------------------------------------
-- Generic minor functions used by engine
-- FIXME: We should be able to abstract these functions away with cleaner Haskell

justs :: [Maybe a] -> [a]
justs []            = []
justs ((Just x):xs) = x:(justs xs)
justs (Nothing:xs)  = justs xs

unMaybe :: String -> Maybe Bindings -> Bindings
unMaybe controllerStr Nothing     = falseBindings controllerStr
unMaybe controllerStr (Just xs)   = xs
          
maybeCons :: Maybe Bindings -> [Maybe Bindings] -> [Maybe Bindings]
maybeCons (Just x) []               = []
maybeCons (Just x) ((Just y):ys)    = (Just (x `truthListAppend` y)):(maybeCons (Just x) ys)
maybeCons (Just x) (Nothing:ys)     = Nothing:(maybeCons (Just x) ys)
maybeCons Nothing  ys               = ys

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left a)  = Nothing
                        
disunityToMaybe :: Bindings -> Maybe Bindings
-- disunityToMaybe (TruthList disunity xs) = Nothing
disunityToMaybe x = Just x
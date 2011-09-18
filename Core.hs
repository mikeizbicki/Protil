module Core 
    ( prove
    ) where

import Data.Monoid

import DataTypes
import LogicBoolean
import Parser

-- Solver below

prove :: Rules -> [Term] -> [Bindings]
prove rules goals = extractValidBindings $ prove' rules 1 goals
    where extractValidBindings xs = fmap (\x -> TruthList (truthVal x) $ filter (not . isBindingAuto) (truthList x)) $ filter (\x -> truthVal x /= disunity prolog) $ justs xs

prove' :: Rules -> Int -> Terms -> [Maybe Bindings]
prove' rules i goals = do
    rule' <- decorateRules i rules
    let (newBindings, newGoals) = branch prolog rule' goals
    let answer = if newGoals == []
                    then [newBindings]
                    else if maybeTruth newBindings == disunity prolog-- newBindings==Nothing
                            then [Nothing]
                            else maybeCons newBindings (prove' rules (i+1) newGoals)
    answer
        where maybeTruth (Just (TruthList truth list)) = truth

branch :: TruthController -> Rule -> Terms -> (Maybe Bindings, Terms)
-- branch rule goals | trace ("branch " ++ show rule ++ " " ++ show goals) False = undefined
branch truthController rule (goal:goals) = (newBindings, sub (unMaybe newBindings) (newGoals++goals))
    where Rule nextTerm body truthValue = rule
          newGoals = body
          newBindings = disunityToMaybe $ unifyTerms truthController nextTerm goal

unifyTerms :: TruthController -> Term -> Term -> Bindings
unifyTerms truthController (CTerm _ []) (CTerm _ []) = TruthList (defaultTruthValue truthController) []
unifyTerms truthController (CTerm _ []) (CTerm _ _)  = falseBindings truthController 
unifyTerms truthController (CTerm _ _ ) (CTerm _ []) = falseBindings truthController 
unifyTerms truthController (CTerm f1 (x:xs)) (CTerm f2 (y:ys))
    | f1 /= f2  = falseBindings truthController 
    | length xs /= length ys = falseBindings truthController 
    | hasVar x  = 
        let rest = unifyTerms truthController (subTerm (x,y) t1) (subTerm (x,y) t2) in
            (TruthList (truthOfInference truthController) [(x,y)]) `mappend` rest
    | hasVar y  = 
        let rest = unifyTerms truthController (subTerm (y,x) t1) (subTerm (y,x) t2) in
            (TruthList (truthOfInference truthController) [(y,x)]) `mappend` rest
    | x == y    = unifyTerms truthController t1 t2
    | otherwise = falseBindings truthController 
    where
          t1 = CTerm f1 xs
          t2 = CTerm f2 ys
unifyTerms truthController a1 a2 = error ( "Non-exhaustive blah: a1=" ++ (show a1) ++ " a2=" ++ (show a2))

-- unifyTerms :: Term -> Term -> Either String Bindings
-- unifyTerms (CTerm _ []) (CTerm _ []) = Right (TruthList defaultTruthValue [])
-- unifyTerms (CTerm f1 (x:xs)) (CTerm f2 (y:ys))
--     | f1 /= f2  = Left $ "unifyTerms failed: functor " ++ (show f1) ++ " /= " ++ (show f2)
--     | length xs /= length ys = Left $ "unifyTerms failed: different number args"
--     | hasVar x  = do
--         rest <- unifyTerms (subTerm (x,y) t1) (subTerm (x,y) t2) in 
--         (TruthList defaultTruthValue [(x,y)]) `mappend` rest
-- {-    | hasVar x  = do
--         rest <- unifyTerms (subTerm (x,y) t1) (subTerm (x,y) t2)
--         return ((x,y):rest)-}
--     | hasVar y  = do
--         rest <- unifyTerms (subTerm (y,x) t1) (subTerm (y,x) t2)
--         return ((y,x):rest)
--     | x == y    = unifyTerms t1 t2
--     | otherwise = Left $ "unifyTerms failed: atoms " ++ (show x) ++ " /= " ++ (show y)
--     where
--           t1 = CTerm f1 xs
--           t2 = CTerm f2 ys
-- unifyTerms a1 a2 = error ( "Non-exhaustive blah: a1=" ++ (show a1) ++ " a2=" ++ (show a2))

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


-- Maybe utils used by solving engine

justs :: [Maybe a] -> [a]
justs []            = []
justs ((Just x):xs) = x:(justs xs)
justs (Nothing:xs)  = justs xs

unMaybe :: (Monoid a) => Maybe a -> a
unMaybe Nothing     = mempty
unMaybe (Just xs)   = xs
          
maybeCons :: Maybe Bindings -> [Maybe Bindings] -> [Maybe Bindings]
maybeCons (Just x) []               = []
maybeCons (Just x) ((Just y):ys)    = (Just (x `mappend` y)):(maybeCons (Just x) ys)
maybeCons (Just x) (Nothing:ys)     = Nothing:(maybeCons (Just x) ys)
maybeCons Nothing  ys               = ys

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left a)  = Nothing
                        
disunityToMaybe :: Bindings -> Maybe Bindings
-- disunityToMaybe (TruthList disunity xs) = Nothing
disunityToMaybe x = Just x
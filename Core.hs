module Core 
    ( prove
    ) where

import DataTypes
import Logic

-------------------------------------------------------------------------------
-- Main solving functions
--
-- FIXME: make this code actually readable
--        flowchart the logic

prove :: RulesDB -> [Term] -> [TruthList]
prove rulesDB goals = extractValidBindings $ prove' rulesDB 1 goals
    where extractValidBindings xs = fmap (\x -> TruthList (truthVal x) $ filter (not . isBindingAuto) (truthList x)) $ filter (\x -> truthVal x /= (tracerFetch (dbTruthController rulesDB) "disunity")) $ justs xs

prove' :: RulesDB -> Int -> [Term] -> [Maybe TruthList]
prove' rulesDB i goals = do
    rule' <- decorateRules i (dbRules rulesDB)
    let (newBindings, newGoals) = branch rulesDB rule' goals
    let answer = if newGoals == []
                    then [newBindings]
                    else if maybeTruth newBindings == tracerFetch (dbTruthController rulesDB) "disunity"
                            then [Nothing]
                            else maybeCons newBindings (prove' rulesDB (i+1) newGoals)
    answer
        where maybeTruth (Just (TruthList truth list)) = truth

branch :: RulesDB -> Rule -> [Term] -> (Maybe TruthList, [Term])
branch rulesDB rule (goal:goals) = (newBindings, sub (unMaybe (dbTruthController rulesDB) newBindings) (newGoals++goals))
    where (Rule nextTerm body truthValue) = rule
          newGoals = body
          newBindings = disunityToMaybe $ unifyTerms rulesDB truthValue nextTerm goal

unifyTerms :: RulesDB -> TracerBox -> Term -> Term -> TruthList
unifyTerms rulesDB baseTruth (CTerm x []) (CTerm y []) = 
    if x==y
        then TruthList baseTruth []
        else error $ "unified non-same terms ?! : " ++ (show x) ++ " " ++ (show y)
unifyTerms rulesDB baseTruth (CTerm _ []) (CTerm _ _ ) = falseBindings (dbTruthController rulesDB) 
unifyTerms rulesDB baseTruth (CTerm _ _ ) (CTerm _ []) = falseBindings (dbTruthController rulesDB) 
unifyTerms rulesDB baseTruth (CTerm f1 (x:xs)) (CTerm f2 (y:ys))
    | f1 /= f2  = falseBindings (dbTruthController rulesDB) 
    | length xs /= length ys = falseBindings (dbTruthController rulesDB) 
    | hasVar x  = 
        let rest = unifyTerms rulesDB baseTruth (subTerm (x,y) t1) (subTerm (x,y) t2) in
            (TruthList (tracerFetch (dbTruthController rulesDB) "truthOfInference") [(x,y)]) `truthListAppend` rest
    | hasVar y  = 
        let rest = unifyTerms rulesDB baseTruth (subTerm (y,x) t1) (subTerm (y,x) t2) in
            (TruthList (tracerFetch (dbTruthController rulesDB) "truthOfInference") [(y,x)]) `truthListAppend` rest
    | x == y    = unifyTerms rulesDB baseTruth t1 t2
    | otherwise = falseBindings (dbTruthController rulesDB) 
    where
          t1 = CTerm f1 xs
          t2 = CTerm f2 ys
unifyTerms rulesDB baseTruth a1 a2 = error ( "Non-exhaustive blah: a1=" ++ (show a1) ++ " a2=" ++ (show a2))

---------------------------------------
-- RulesDB functions

findRule :: RulesDB -> String -> Rule
findRule (RulesDB controller []    ) ruleStr = error $ "Rule " ++ ruleStr ++ " not found!"
findRule (RulesDB controller (x:xs)) ruleStr = if (ruleL x) == Atom ruleStr
                                                then x
                                                else findRule (RulesDB controller xs) ruleStr

---------------------------------------
-- Minor functions for unification

sub :: TruthList -> [Term] -> [Term]
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

decorateRules :: Int -> [Rule] -> [Rule]
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

falseBindings :: String -> TruthList
falseBindings controlStr = TruthList (tracerFetch controlStr "disunity") []

isBindingAuto :: Binding -> Bool
isBindingAuto (Var v, _) = '@' `elem` v
isBindingAuto _          = False

---------------------------------------
-- Generic minor functions used by engine
-- FIXME: We should be able to abstract these functions away with cleaner Haskell

justs :: [Maybe a] -> [a]
justs []            = []
justs ((Just x):xs) = x:(justs xs)
justs (Nothing:xs)  = justs xs

unMaybe :: String -> Maybe TruthList -> TruthList
unMaybe controllerStr Nothing     = falseBindings controllerStr
unMaybe controllerStr (Just xs)   = xs
          
maybeCons :: Maybe TruthList -> [Maybe TruthList] -> [Maybe TruthList]
maybeCons (Just x) []               = []
maybeCons (Just x) ((Just y):ys)    = (Just (x `truthListAppend` y)):(maybeCons (Just x) ys)
maybeCons (Just x) (Nothing:ys)     = Nothing:(maybeCons (Just x) ys)
maybeCons Nothing  ys               = ys

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left a)  = Nothing
                        
disunityToMaybe :: TruthList -> Maybe TruthList
-- disunityToMaybe (TruthList disunity xs) = Nothing
disunityToMaybe x = Just x
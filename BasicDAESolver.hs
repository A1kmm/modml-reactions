{-# LANGUAGE BangPatterns #-}
module BasicDAESolver
where

import BasicDAEModel
import Control.Monad
import Control.Monad.Error
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics
import Control.Monad.State
import Data.Maybe

data CodeGenerationError = OtherProblem String
instance Error CodeGenerationError where strMsg s = OtherProblem s
type AllowCodeGenError a = Either CodeGenerationError a

makeCodeFor :: BasicDAEModel -> AllowCodeGenError String
makeCodeFor mod =
  do
    let mod' = removeUnusedVariablesAndCSEs (simplifyDerivatives mod)
    let (varCount, varNumMap) = numberVariables (variables mod')
    return $ makeResFn mod' varCount varNumMap

makeResFn mod varCount varNumMap =
  let
      considerCSEIn = (equations mod, forcedInequalities mod, checkedConditions mod)
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod varNumMap usedRealCSEs
      (_, boolCSEMap, _, boolcses) = buildBoolCSEs mod varNumMap cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    "int modelResiduals(double time, N_Vector y, N_Vector derivy, N_Vector resids, void* user_data)\n\
    \{\n" ++ 
    realcses ++ boolcses ++
    (makeResiduals mod varCount varNumMap realCSEMap boolCSEMap) ++
    (makeConditionChecks mod varNumMap realCSEMap boolCSEMap) ++
    "\n}"

escapeCString [] = []
escapeCString (c:s)
  | c == '\n' = '\\':'n':(escapeCString s)
  | c == '\r' = '\\':'n':(escapeCString s)
  | c == '\\' = '\\':'\\':(escapeCString s)
  | c == '\"' = '\\':'\"':(escapeCString s)
  | otherwise = c:(escapeCString s)

oneConditionCheck :: M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> (String, BoolExpression) -> String
oneConditionCheck varNumMap realCSEMap boolCSEMap (msg, cond) =
    "if (" ++ (boolExpressionToString varNumMap realCSEMap boolCSEMap cond) ++ ")\n\
    \{\n\
    \  checkedConditionFail(\"" ++ (escapeCString msg) ++ "\");\n\
    \  return -1;\n\
    \}\n"

makeConditionChecks (BasicDAEModel { checkedConditions = ccList}) varNumMap realCSEMap boolCSEMap =
    let
        x :: [(String, BoolExpression)]
        x = ccList
    in
      concatMap (oneConditionCheck varNumMap realCSEMap boolCSEMap) x

inequalityResidual varMap realCSEMap boolCSEMap ieq = " + max(0.0, " ++ (realExpressionToString varMap realCSEMap boolCSEMap ieq) ++ ")"
equationToResidual n varNumMap realCSEMap boolCSEMap (RealEquation e1 e2) suf =
    (showString "res[" . shows n . showString "] = (" . showString (realExpressionToString varNumMap realCSEMap boolCSEMap e1)
                . showString ") - ("
                . showString (realExpressionToString varNumMap realCSEMap boolCSEMap e2)
                . showString ")" . showString suf) ";\n"

buildRealCSE m varNumMap cse@(RealCommonSubexpression id ex) (nalloc, realCSEMap, boolCSEMap, s) 
    | isJust $ M.lookup cse realCSEMap = (nalloc, realCSEMap, boolCSEMap, s)
    | otherwise =
        let
            neededRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) ex
            (nalloc', realCSEMap', boolCSEMap', s') =
                 S.fold (buildRealCSE m varNumMap) (nalloc, realCSEMap, boolCSEMap, s) neededRealCSEs
            neededBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) ex
            (nalloc'', boolCSEMap'', realCSEMap'', s'') =
                 S.fold (buildBoolCSE m varNumMap) (nalloc', boolCSEMap', realCSEMap', s') neededBoolCSEs
            v = "c" ++ show nalloc''
        in
          (nalloc'' + 1, M.insert cse v realCSEMap', boolCSEMap'',
           s'' ++ "double " ++ v ++ " = " ++ (realExpressionToString varNumMap realCSEMap'' boolCSEMap'' ex) ++ ";\n")

buildBoolCSE :: BasicDAEModel -> M.Map RealVariable Int -> BoolCommonSubexpression -> (Int, M.Map BoolCommonSubexpression String, M.Map RealCommonSubexpression String, String) -> (Int, M.Map BoolCommonSubexpression String, M.Map RealCommonSubexpression String, String)
buildBoolCSE m varNumMap cse@(BoolCommonSubexpression id ex) (nalloc, boolCSEMap, realCSEMap, s)
    | isJust $ M.lookup cse boolCSEMap = (nalloc, boolCSEMap, realCSEMap, s)
    | otherwise =
        let
            neededBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) ex
            (nalloc', boolCSEMap', realCSEMap', s') =
                 S.fold (buildBoolCSE m varNumMap) (nalloc, boolCSEMap, realCSEMap, s) neededBoolCSEs
            neededRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) ex
            (nalloc'', realCSEMap'', boolCSEMap'', s'') =
                 S.fold (buildRealCSE m varNumMap) (nalloc', realCSEMap', boolCSEMap', s') neededRealCSEs
            v = "c" ++ show nalloc''
        in
          (nalloc'' + 1, M.insert cse v boolCSEMap'', realCSEMap'',
           s'' ++ "bool " ++ v ++ " = " ++ (boolExpressionToString varNumMap realCSEMap'' boolCSEMap'' ex) ++ ";\n")

buildRealCSEs model varNumMap = S.fold (buildRealCSE model varNumMap) (0, M.empty, M.empty, "")
buildBoolCSEs model varNumMap cseNo realCSEMap boolCSEMap =
    S.fold (buildBoolCSE model varNumMap) (cseNo, boolCSEMap, realCSEMap, "")

boolExpressionToString :: M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> BoolExpression -> String
boolExpressionToString _ _ _ (BoolConstant True) = "1"
boolExpressionToString _ _ _ (BoolConstant False) = "0"
boolExpressionToString _ _ bcem (BoolCommonSubexpressionE bce) = (M.!) bcem bce
boolExpressionToString vm rcem bcem (be1 `And` be2) =
    (showString ('(':(boolExpressionToString vm rcem bcem be1)) . showString (")&&(") . showString (boolExpressionToString vm rcem bcem be2)
                    . showString ")") ""

boolExpressionToString vm rcem bcem (be1 `Or` be2) =
    (showString ('(':(boolExpressionToString vm rcem bcem be1)) . showString (")||(") . showString (boolExpressionToString vm rcem bcem be2)
                    . showString ")") ""

boolExpressionToString vm rcem bcem (Not be1) =
    (showString ('!':'(':(boolExpressionToString vm rcem bcem be1)) . showString (")")) ""

boolExpressionToString vm rcem bcem (re1 `LessThan` re2) =
    (showString ('(':(realExpressionToString vm rcem bcem be1)) . showString (")||(") . showString (boolExpressionToString vm rcem bcem be2)
                    . showString ")") ""



realExpressionToString :: M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> RealExpression -> String
realExpressionToString = undefined

makeResiduals (BasicDAEModel { equations = [] }) _ _ _ _ = ""
makeResiduals m@(BasicDAEModel { equations = e1:erest, forcedInequalities = ieqs }) varCount varNumMap realCSEMap boolCSEMap =
    -- The first equation is special, because the inequalities are appended to it.
    (equationToResidual 0 varNumMap realCSEMap boolCSEMap e1
                            (concatMap (inequalityResidual varNumMap realCSEMap boolCSEMap) ieqs)) ++
    concatMap (\(i,e) -> equationToResidual i varNumMap realCSEMap boolCSEMap e "") (zip [1..] erest)

simplifyDerivatives mod =
  snd $
    until ((==0) . fst) (\(_, mod') -> simplifyDerivativesRound mod' (buildVarMap mod')) (1, mod)
simplifyDerivativesRound mod varmap =
    (count, mod' { variables = varlist, nextID = nextIDv})
    where
      (mod', (count, varmap', varlist, nextIDv)) =
          runState (everywhereM (mkM simplifyOneDerivative) mod) (0, varmap, variables mod, nextID mod)

simplifyOneDerivative d@(Derivative (RealVariableE _)) = return d
simplifyOneDerivative (Derivative ex) =
  do
    (count, varmap, varlist, id) <- get
    let mv = M.lookup ex varmap
    let (v, varmap', varlist', id') = case mv
         of
           Just v -> (v, varmap, varlist, id)
           Nothing ->
            (v, M.insert ex v varmap, v:varlist, id - 1)
              where
                v = RealVariable id
    put (count + 1, varmap', varlist', id')
    return $ Derivative (RealVariableE v)
simplifyOneDerivative d = return d

buildVarMap m = foldl' processEquationForVarMap M.empty (equations m)
processEquationForVarMap st (RealEquation (RealVariableE var) ex) =
    M.insert ex var st
processEquationForVarMap st (RealEquation ex1 ex2@(RealVariableE var)) = processEquationForVarMap st (RealEquation ex2 ex1)
processEquationForVarMap st _ = st

removeUnusedVariablesAndCSEs mod =
  let
    usedRealVars = everything S.union (S.empty `mkQ` findOneUsedRealVariable) (equations mod)
    considerCSEIn = (equations mod, interventionRoots mod, forcedInequalities mod, checkedConditions mod)
    usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
    usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
  in
    mod { variables = S.toList $ usedRealVars `S.union` (S.fromList (variables mod)) }

findOneUsedRealVariable :: RealVariable -> S.Set RealVariable
findOneUsedRealVariable = S.singleton
findOneUsedRealCSE :: RealCommonSubexpression -> S.Set RealCommonSubexpression
findOneUsedRealCSE = S.singleton
findOneUsedBoolCSE :: BoolCommonSubexpression -> S.Set BoolCommonSubexpression
findOneUsedBoolCSE = S.singleton

numberVariables vars =
    (length vars, M.fromList $ zip vars [0..])

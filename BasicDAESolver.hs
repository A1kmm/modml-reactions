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
instance Show CodeGenerationError where showsPrec _ (OtherProblem s) = showString "Error: " . showString s
type AllowCodeGenError a = Either CodeGenerationError a

makeCodeFor :: BasicDAEModel -> AllowCodeGenError String
makeCodeFor mod =
  do
    let mod' = removeUnusedVariablesAndCSEs (simplifyDerivatives mod)
    let (varCount, varNumMap) = numberVariables (variables mod')
    return $ "#include \"SolverHead.h\"\n" ++
             (makeResFn mod' varCount varNumMap) ++
             (makeRootFn mod' varCount varNumMap) ++
             (makeVarId mod') ++
             (makeSettings mod' varCount)

makeVarId mod =
  let
      varsWithDerivs = everything S.union (S.empty `mkQ` findOneDeriv) (equations mod)
  in
    "static void setupIdVector(N_Vector id)\n\
    \{\n\
    \  double *r = N_VGetArrayPointer(id);\n" ++
    concatMap (\(i, v) -> (showString "r["
                            . shows i
                            . showString "] = " 
                            . showString (boolToDoubleStr $ S.member v varsWithDerivs)) ";\n")
      (zip [0..] $ variables mod) ++
    "}\n"

findOneDeriv (Derivative (RealVariableE v)) = S.singleton v
findOneDeriv _ = S.empty
boolToDoubleStr True = "1.0"
boolToDoubleStr False = "0.0"

makeSettings mod varCount =
  (showString "static int gNumVars = " . shows varCount . showString ";\n\
   \static int gNumInterventions = " . shows (length $ interventionRoots mod)) ";\n"

makeResFn mod varCount varNumMap =
  let
      considerCSEIn = (equations mod, forcedInequalities mod, checkedConditions mod)
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod varNumMap usedRealCSEs
      (_, boolCSEMap, _, boolcses) = buildBoolCSEs mod varNumMap cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    "static int modelResiduals(double t, N_Vector y, N_Vector derivy, N_Vector resids, void* user_data)\n\
    \{\n\
    \  double* res = N_VGetArrayPointer(resids),\n\
    \        * v = N_VGetArrayPointer(y),\n\
    \        * dv = N_VGetArrayPointer(derivy);\n" ++
    realcses ++ boolcses ++
    (makeResiduals mod varCount varNumMap realCSEMap boolCSEMap) ++
    (makeConditionChecks mod varNumMap realCSEMap boolCSEMap) ++
    "  return 0;\n\
    \}\n"

makeRootFn mod varCount varNumMap =
  let
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) (interventionRoots mod)
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) (interventionRoots mod)
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod varNumMap usedRealCSEs
      (_, boolCSEMap, _, boolcses) = buildBoolCSEs mod varNumMap cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    "static int modelRoots(double t, N_Vector y, N_Vector derivy, realtype *gout, void *user_data)\n\
    \{\n\
    \  double* v = N_VGetArrayPointer(y),\n\
    \        * dv = N_VGetArrayPointer(derivy);\n" ++
    realcses ++ boolcses ++
    (makeInterventionRoots mod varNumMap realCSEMap boolCSEMap) ++
    "  return 0;\n\
    \}\n"

makeInterventionRoots :: BasicDAEModel -> M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> String
makeInterventionRoots mod varMap realCSEMap boolCSEMap =
    concatMap (makeOneInterventionRoot varMap realCSEMap boolCSEMap) (zip [0..] (interventionRoots mod))

makeOneInterventionRoot varMap realCSEMap boolCSEMap (i, root) =
    (showString "gout[" . shows i . showString "] = " .
       showString (realExpressionToString varMap realCSEMap boolCSEMap root)) ";\n"

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

inequalityResidual varMap realCSEMap boolCSEMap ieq = "-min(0, " ++ (realExpressionToString varMap realCSEMap boolCSEMap ieq) ++ ")"
makeResidual n str =
    (showString "res[" . shows n . showString "] = " . showString str) ";\n"

equationToResidualString varNumMap realCSEMap boolCSEMap (RealEquation e1 e2) =
    (showString "((" . showString (realExpressionToString varNumMap realCSEMap boolCSEMap e1)
                . showString ") - ("
                . showString (realExpressionToString varNumMap realCSEMap boolCSEMap e2)
    ) "))"

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
           s'' ++ "int " ++ v ++ " = " ++ (boolExpressionToString varNumMap realCSEMap'' boolCSEMap'' ex) ++ ";\n")

buildRealCSEs model varNumMap = S.fold (buildRealCSE model varNumMap) (0, M.empty, M.empty, "")
buildBoolCSEs model varNumMap cseNo realCSEMap boolCSEMap =
    S.fold (buildBoolCSE model varNumMap) (cseNo, boolCSEMap, realCSEMap, "")

boolExpressionToString :: M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> BoolExpression -> String
boolExpressionToString _ _ _ (BoolConstant True) = "1"
boolExpressionToString _ _ _ (BoolConstant False) = "0"
boolExpressionToString _ _ bcem (BoolCommonSubexpressionE bce) = (M.!) bcem bce
boolExpressionToString vm rcem bcem (be1 `And` be2) =
    (showString ('(':(boolExpressionToString vm rcem bcem be1)) 
     . showString (")&&(") 
     . showString (boolExpressionToString vm rcem bcem be2)) ")"

boolExpressionToString vm rcem bcem (be1 `Or` be2) =
    (showString ('(':(boolExpressionToString vm rcem bcem be1))
     . showString (")||(")
     . showString (boolExpressionToString vm rcem bcem be2)) ")"

boolExpressionToString vm rcem bcem (Not be1) =
    (showString ('!':'(':(boolExpressionToString vm rcem bcem be1))) ")"

boolExpressionToString vm rcem bcem (re1 `LessThan` re2) =
    (showString ('(':(realExpressionToString vm rcem bcem re1))
       . showString (")<(") 
       . showString (realExpressionToString vm rcem bcem re2)) ")"
boolExpressionToString vm rcem bcem (re1 `Equal` re2) =
    (showString ('(':(realExpressionToString vm rcem bcem re1)) 
     . showString (")==(") 
     . showString (realExpressionToString vm rcem bcem re2)) ")"

realExpressionToString :: M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> RealExpression -> String
realExpressionToString _ _ _ (RealConstant c) = show c
realExpressionToString vm _ _ (RealVariableE v) = (showString "v[" . shows ((M.!) vm v)) "]"
realExpressionToString _ _ _ (BoundVariableE) = "t"
realExpressionToString vm _ _ (Derivative (RealVariableE v)) = (showString "dv[" . shows ((M.!) vm v) . showString "]") ""
realExpressionToString _ rcem _ (RealCommonSubexpressionE cse) = (M.!) rcem cse
realExpressionToString vm rcem bcem (If b1 r1 r2) =
  (showString "("
   . showString (boolExpressionToString vm rcem bcem b1)
   . showString ") ? ("
   . showString (realExpressionToString vm rcem bcem r1)
   . showString ") : (" 
   . showString (realExpressionToString vm rcem bcem r2)) ")"
realExpressionToString vm rcem bcem (r1 `Plus` r2) =
    (showString ('(':(realExpressionToString vm rcem bcem r1)) 
     . showString (")+(") 
     . showString (realExpressionToString vm rcem bcem r2)) ")"
realExpressionToString vm rcem bcem (r1 `Minus` r2) =
    (showString ('(':(realExpressionToString vm rcem bcem r1)) 
     . showString (")-(") 
     . showString (realExpressionToString vm rcem bcem r2)) ")"
realExpressionToString vm rcem bcem (r1 `Times` r2) =
    (showString ('(':(realExpressionToString vm rcem bcem r1)) 
     . showString (")*(") 
     . showString (realExpressionToString vm rcem bcem r2)) ")"
realExpressionToString vm rcem bcem (r1 `Divided` r2) =
    (showString ('(':(realExpressionToString vm rcem bcem r1)) 
     . showString (")/(") 
     . showString (realExpressionToString vm rcem bcem r2)) ")"
realExpressionToString vm rcem bcem (r1 `Power` r2) =
    (showString "pow(("
      . showString (realExpressionToString vm rcem bcem r1)
      . showString ("), (") 
      . showString (realExpressionToString vm rcem bcem r2)) "))"
realExpressionToString vm rcem bcem (Floor r1) = 
    (showString "floor("
     . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (Ceiling r1) =
    (showString "ceiling("
     . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (LogBase r1 r2) =
    (showString "log("
      . showString (realExpressionToString vm rcem bcem r2)
      . showString (") / log(") 
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (Sin r1) =
    (showString "sin("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (Tan r1) =
    (showString "tan("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (Cos r1) =
    (showString "cos("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (ASin r1) =
    (showString "asin("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (ATan r1) =
    (showString "atan("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (ACos r1) =
    (showString "acos("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (Sinh r1) =
    (showString "sinh("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (Tanh r1) =
    (showString "tanh("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (Cosh r1) =
    (showString "cosh("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (ASinh r1) =
    (showString "asinh("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (ATanh r1) =
    (showString "atanh("
      . showString (realExpressionToString vm rcem bcem r1)) ")"
realExpressionToString vm rcem bcem (ACosh r1) =
    (showString "acosh("
      . showString (realExpressionToString vm rcem bcem r1)) ")"

intermixLists' l [] [] = l
intermixLists' l a [] = a ++ l
intermixLists' l [] b = b ++ l
intermixLists' l (a1:al) (b1:bl) = intermixLists' (a1:b1:l) al bl

intermixLists = intermixLists' []

makeResiduals :: BasicDAEModel -> Int -> M.Map RealVariable Int -> M.Map RealCommonSubexpression String ->
                 M.Map BoolCommonSubexpression String -> String
makeResiduals m@(BasicDAEModel { equations = eqns, forcedInequalities = ieqs }) varCount varNumMap realCSEMap boolCSEMap =
    let
        ieqStrList = map (inequalityResidual varNumMap realCSEMap boolCSEMap) ieqs
        eqnStrList = map (equationToResidualString varNumMap realCSEMap boolCSEMap) eqns
        strList = intermixLists ieqStrList eqnStrList
        sll = ceiling ((fromIntegral (length strList))/(fromIntegral varCount))
    in
      fst $ foldl' (processResidualGroup sll) ("", strList) [0..(varCount - 1)]

joinResiduals [] = ""
joinResiduals (a:[]) = a
joinResiduals (a:l) = "smax(" ++ a ++ ", " ++ (joinResiduals l) ++ ")"

processResidualGroup sll (s, l) i =
    let
        lfront = take sll l
        lback = drop sll l
    in
      (s ++ makeResidual i (joinResiduals lfront), lback)

simplifyDerivatives mod =
  snd $
    until ((==0) . fst) (\(_, mod') -> simplifyDerivativesRound mod' (buildVarMap mod')) (1, mod)
simplifyDerivativesRound mod varmap =
    (count, mod' { variables = varlist, nextID = nextIDv, equations = equationlist ++ (equations mod')})
    where
      (mod', (count, varmap', varlist, equationlist, nextIDv)) =
          runState (everywhereM (mkM simplifyOneDerivative) mod) (0, varmap, variables mod, [], nextID mod)

simplifyOneDerivative d@(Derivative (RealVariableE _)) = return d
simplifyOneDerivative (Derivative ex) =
  do
    ex' <- everywhereM (mkM simplifyOneDerivative) ex
    (count, varmap, varlist, eqnlist, id) <- get
    let mv = M.lookup ex' varmap
    let (v, varmap', varlist', eqnlist', id') = case mv
         of
           Just v -> (v, varmap, varlist, eqnlist, id)
           Nothing ->
            (v, M.insert ex' v varmap, v:varlist, (RealEquation (RealVariableE v) ex'):eqnlist, id - 1)
              where
                v = RealVariable id
    put (count + 1, varmap', varlist', eqnlist', id')
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
    mod { variables = S.toList $ usedRealVars,
          commonSubexpressions = map FromRealCommonSubexpression (S.toList $ usedRealCSEs) ++
                                 map FromBoolCommonSubexpression (S.toList $ usedBoolCSEs) }

findOneUsedRealVariable :: RealVariable -> S.Set RealVariable
findOneUsedRealVariable = S.singleton
findOneUsedRealCSE :: RealCommonSubexpression -> S.Set RealCommonSubexpression
findOneUsedRealCSE = S.singleton
findOneUsedBoolCSE :: BoolCommonSubexpression -> S.Set BoolCommonSubexpression
findOneUsedBoolCSE = S.singleton

numberVariables vars =
    (length vars, M.fromList $ zip vars [0..])

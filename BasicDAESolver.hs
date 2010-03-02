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
import Text.ParserCombinators.Parsec
import qualified System.IO.Unsafe as S
import qualified System.Process as S
import System.FilePath
import System.Unix.Directory
import System.Process
import System.Exit

data CodeGenerationError = OtherProblem String
instance Error CodeGenerationError where strMsg s = OtherProblem s
instance Show CodeGenerationError where showsPrec _ (OtherProblem s) = showString "Error: " . showString s
type AllowCodeGenError a = Either CodeGenerationError a

data IntegrationResult = FatalError (Int, String, String, String) |
                         Warning (Int, String, String, String) |
                         CheckedConditionFail String |
                         Result (Double, [Double], [Double]) |
                         Success deriving (Show, Read)

data SolverParameters = SolverParameters {tStart :: Double,
                                          maxSolverStep :: Double,
                                          maxReportStep :: Double,
                                          tEnd :: Double,
                                          showEveryStep :: Double,
                                          reltol :: Double,
                                          abstol :: Double }
defaultSolverParameters = SolverParameters 0 1 0.1 10 1 1E-6 1E-6

otherProjectsPath = "other-projects"
sundialsPath = otherProjectsPath </> "sundials-2.4.0"
levmarPath = otherProjectsPath </> "levmar-2.5"

compileCodeGetResults params code = 
    withTemporaryDirectory "./" $ \dir ->
      do
        let codegenc = dir </> "codegen.c"
        let codegenx = "/tmp" </> "codegen"
        writeFile codegenc code
        ret <- rawSystem "gcc" ["-O3", "-I", ".",
                                "-I", levmarPath,
                                "-L", levmarPath,
                                "-I", sundialsPath </> "include",
                                "-L", sundialsPath </> "src/nvec_ser/.libs/",
                                "-L", sundialsPath </> "src/ida/.libs/",
                                codegenc, 
                                "-lsundials_ida", "-lsundials_nvecserial", "-lm",
                                 "-llapack", "-llevmar",
                                "-o", codegenx]
        case ret
          of
            ExitFailure _ -> return $ Left (strMsg "Model doesn't compile")
            ExitSuccess ->
                do
                  (_, inp, _) <- readProcessWithExitCode codegenx
                                  (map (\a -> show $ a params)
                                         [tStart, maxSolverStep, maxReportStep, tEnd, showEveryStep, reltol, abstol]
                                  ) ""
                  return (return (read inp))

modelToResults :: BasicDAEModel -> SolverParameters -> AllowCodeGenError (M.Map RealVariable Int, [IntegrationResult])
modelToResults mod params =
    do
        (varmap, code) <- makeCodeFor mod
        res <- S.unsafePerformIO $ compileCodeGetResults params code
        return (varmap, res)

makeCodeFor :: BasicDAEModel -> AllowCodeGenError (M.Map RealVariable Int, String)
makeCodeFor mod =
  do
    let mod' = removeUnusedVariablesAndCSEs (simplifyDerivatives mod)
    let (varCount, varNumMap) = numberVariables (variables mod')
    let (paramNumMap, paramCount) = numberParameters mod'
    return $ (varNumMap,
              "#include \"SolverHead.h\"\n" ++
              (makeResFn mod' varNumMap) ++
              (makeBoundaryResFn mod' paramNumMap) ++
              (makeTranslateParams varNumMap paramNumMap) ++
              (makeRootFn mod' varNumMap) ++
              (makeVarId mod') ++
              (makeSettings mod' varCount paramCount) ++
              (makeConditionChecks mod varNumMap)
             )

makeTranslateParams varNumMap paramNumMap =
    "void translateParams(N_Vector y, N_Vector derivy, double* params)\n\
    \{\n\
    \ double * v = N_VGetArrayPointer(y),\n\
    \        * dv = N_VGetArrayPointer(derivy);\n" ++
    (concatMap (translateOneParam varNumMap) (M.toList paramNumMap)) ++ "\n\
    \}\n" ++
    "void reverseTranslateParams(N_Vector y, N_Vector derivy, double* params)\n\
    \{\n\
    \ double * v = N_VGetArrayPointer(y),\n\
    \        * dv = N_VGetArrayPointer(derivy);\n" ++
    (concatMap (reverseTranslateOneParam varNumMap) (M.toList paramNumMap)) ++ "\n\
    \}\n"

translateOneParam varNumMap ((RealVariableE v), paramNo) =
  "v" ++ (translateOneParamVar varNumMap v paramNo)
translateOneParam varNumMap ((Derivative (RealVariableE v)), paramNo) =
  "dv" ++ (translateOneParamVar varNumMap v paramNo)

translateOneParamVar varNumMap v paramNo =
  (showString "[" . shows ((M.!) varNumMap v) . showString "] = params[" .
   shows paramNo) "];\n"

reverseTranslateOneParam varNumMap ((RealVariableE v), paramNo) =
  (reverseTranslateOneParamVar "v" varNumMap v paramNo)
reverseTranslateOneParam varNumMap ((Derivative (RealVariableE v)), paramNo) =
  (reverseTranslateOneParamVar "dv" varNumMap v paramNo)

reverseTranslateOneParamVar n varNumMap v paramNo =
  (showString "params[" . shows paramNo . showString "] = " . showString n . showString "[" . shows ((M.!) varNumMap v)) "];\n"

numberParameters :: BasicDAEModel -> (M.Map RealExpression Int, Int)
numberParameters mod =
  execState (everywhereM (mkM numberOneParameter) mod) (M.empty, 0)

insertExpression e = do
  (paramNumMap, nextNum) <- get
  case (M.lookup e paramNumMap) of
    Nothing -> do
      put (M.insert e nextNum paramNumMap, nextNum + 1)
      return e
    Just _ -> return e

numberOneParameter (e@(Derivative (RealVariableE _))) = insertExpression e
numberOneParameter e@(RealVariableE _) = insertExpression e
numberOneParameter e = return e

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

makeSettings mod varCount paramCount =
  (showString "static int gNumVars = " . shows varCount . showString ";\n\
   \static int gNumEquations = " . shows (length $ equations mod) . showString ";\n\
   \static int gNumBoundaryEquations = " . shows ((length $ boundaryEquations mod) + (length $ forcedInequalities mod)) . showString ";\n\
   \static int gNumParams = " . shows paramCount . showString ";\n\
   \static int gNumInterventions = " . shows (length $ interventionRoots mod)) ";\n"

makeResFn mod varNumMap =
  let
      considerCSEIn = (equations mod, forcedInequalities mod)
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (variableOrDerivDisplay varNumMap) usedRealCSEs
      (_, boolCSEMap, _, boolcses) = buildBoolCSEs mod (variableOrDerivDisplay varNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    "static int modelResiduals(double t, N_Vector y, N_Vector derivy, N_Vector resids, void* user_data)\n\
    \{\n\
    \  double* res = N_VGetArrayPointer(resids),\n\
    \        * v = N_VGetArrayPointer(y),\n\
    \        * dv = N_VGetArrayPointer(derivy);\n" ++
    realcses ++ boolcses ++
    (makeResiduals mod varNumMap realCSEMap boolCSEMap) ++
    "  return 0;\n\
    \}\n"

makeBoundaryResFn mod paramNumMap =
  let
      considerCSEIn = (equations mod, forcedInequalities mod, boundaryEquations mod)
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (paramDisplay paramNumMap) usedRealCSEs
      (_, boolCSEMap, _, boolcses) = buildBoolCSEs mod (paramDisplay paramNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
  in
    "static int boundaryResiduals(double t, double *params, double *res)\n\
    \{\n" ++
    realcses ++ boolcses ++
    (makeBoundaryResiduals mod paramNumMap realCSEMap boolCSEMap) ++
    "  return 0;\n\
    \}\n"

makeRootFn :: BasicDAEModel -> M.Map RealVariable Int -> String
makeRootFn mod varNumMap =
  let
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) (interventionRoots mod)
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) (interventionRoots mod)
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (variableOrDerivDisplay varNumMap) usedRealCSEs
      (_, boolCSEMap, _, boolcses) = buildBoolCSEs mod (variableOrDerivDisplay varNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
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
       showString (realExpressionToString (variableOrDerivDisplay varMap) realCSEMap boolCSEMap root)) ";\n"

escapeCString [] = []
escapeCString (c:s)
  | c == '\n' = '\\':'n':(escapeCString s)
  | c == '\r' = '\\':'n':(escapeCString s)
  | c == '\\' = '\\':'\\':(escapeCString s)
  | c == '\"' = '\\':'\"':(escapeCString s)
  | otherwise = c:(escapeCString s)

oneConditionCheck :: M.Map RealVariable Int -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> (String, BoolExpression) -> String
oneConditionCheck varNumMap realCSEMap boolCSEMap (msg, cond) =
    "if (" ++ (boolExpressionToString (variableOrDerivDisplay varNumMap) realCSEMap boolCSEMap cond) ++ ")\n\
    \{\n\
    \  checkedConditionFail(\"" ++ (escapeCString msg) ++ "\");\n\
    \  return -1;\n\
    \}\n"

makeConditionChecks mod@(BasicDAEModel { checkedConditions = ccList}) varNumMap =
    let
      considerCSEIn = checkedConditions
      usedRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) considerCSEIn
      usedBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) considerCSEIn
      (cseNo, realCSEMap, boolCSEMap', realcses) = buildRealCSEs mod (variableOrDerivDisplay varNumMap) usedRealCSEs
      (_, boolCSEMap, _, boolcses) = buildBoolCSEs mod (variableOrDerivDisplay varNumMap) cseNo realCSEMap boolCSEMap' usedBoolCSEs
    in
      "static int checkConditions(double t, N_Vector y, N_Vector derivy)\n\
      \{\n\
      \  double* v = N_VGetArrayPointer(y),\n\
      \        * dv = N_VGetArrayPointer(derivy);\n" ++
      realcses ++ boolcses ++
      (concatMap (oneConditionCheck varNumMap realCSEMap boolCSEMap) ccList) ++
      "  return 0;\n}\n"

inequalityResidual vnf realCSEMap boolCSEMap ieq = "-min(0, " ++ (realExpressionToString vnf realCSEMap boolCSEMap ieq) ++ ")"
inequalityTest vnf realCSEMap boolCSEMap ieq = "((" ++ (realExpressionToString vnf realCSEMap boolCSEMap ieq) ++ ") > 0)"
makeResidual (n, str) =
    (showString "res[" . shows n . showString "] = " . showString str) ";\n"

equationToResidualString vnf realCSEMap boolCSEMap (RealEquation e1 e2) =
    (showString "((" . showString (realExpressionToString vnf realCSEMap boolCSEMap e1)
                . showString ") - ("
                . showString (realExpressionToString vnf realCSEMap boolCSEMap e2)
    ) "))"

buildRealCSE m vnf cse@(RealCommonSubexpression id ex) (nalloc, realCSEMap, boolCSEMap, s) 
    | isJust $ M.lookup cse realCSEMap = (nalloc, realCSEMap, boolCSEMap, s)
    | otherwise =
        let
            neededRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) ex
            (nalloc', realCSEMap', boolCSEMap', s') =
                 S.fold (buildRealCSE m vnf) (nalloc, realCSEMap, boolCSEMap, s) neededRealCSEs
            neededBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) ex
            (nalloc'', boolCSEMap'', realCSEMap'', s'') =
                 S.fold (buildBoolCSE m vnf) (nalloc', boolCSEMap', realCSEMap', s') neededBoolCSEs
            v = "c" ++ show nalloc''
        in
          (nalloc'' + 1, M.insert cse v realCSEMap', boolCSEMap'',
           s'' ++ "double " ++ v ++ " = " ++ (realExpressionToString vnf realCSEMap'' boolCSEMap'' ex) ++ ";\n")

buildBoolCSE :: BasicDAEModel -> (RealExpression -> String) -> BoolCommonSubexpression -> (Int, M.Map BoolCommonSubexpression String, M.Map RealCommonSubexpression String, String) -> (Int, M.Map BoolCommonSubexpression String, M.Map RealCommonSubexpression String, String)
buildBoolCSE m vm cse@(BoolCommonSubexpression id ex) (nalloc, boolCSEMap, realCSEMap, s)
    | isJust $ M.lookup cse boolCSEMap = (nalloc, boolCSEMap, realCSEMap, s)
    | otherwise =
        let
            neededBoolCSEs = everything S.union (S.empty `mkQ` findOneUsedBoolCSE) ex
            (nalloc', boolCSEMap', realCSEMap', s') =
                 S.fold (buildBoolCSE m vm) (nalloc, boolCSEMap, realCSEMap, s) neededBoolCSEs
            neededRealCSEs = everything S.union (S.empty `mkQ` findOneUsedRealCSE) ex
            (nalloc'', realCSEMap'', boolCSEMap'', s'') =
                 S.fold (buildRealCSE m vm) (nalloc', realCSEMap', boolCSEMap', s') neededRealCSEs
            v = "c" ++ show nalloc''
        in
          (nalloc'' + 1, M.insert cse v boolCSEMap'', realCSEMap'',
           s'' ++ "int " ++ v ++ " = " ++ (boolExpressionToString vm realCSEMap'' boolCSEMap'' ex) ++ ";\n")

buildRealCSEs model vm = S.fold (buildRealCSE model vm) (0, M.empty, M.empty, "")
buildBoolCSEs model vm cseNo realCSEMap boolCSEMap =
    S.fold (buildBoolCSE model vm) (cseNo, boolCSEMap, realCSEMap, "")

boolExpressionToString :: (RealExpression -> String) -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> BoolExpression -> String
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

variableOrDerivDisplay :: M.Map RealVariable Int -> RealExpression -> String
variableOrDerivDisplay vm (RealVariableE v) = (showString "v[" . shows ((M.!) vm v)) "]"
variableOrDerivDisplay vm (Derivative (RealVariableE v)) = (showString "dv[" . shows ((M.!) vm v)) "]"
variableOrDerivDisplay vm _ = undefined

paramDisplay :: M.Map RealExpression Int -> RealExpression -> String
paramDisplay m ex = (showString "params[" . shows ((M.!) m ex)) "]"

realExpressionToString :: (RealExpression -> String) -> M.Map RealCommonSubexpression String -> M.Map BoolCommonSubexpression String -> RealExpression -> String
realExpressionToString _ _ _ (RealConstant c) = show c
realExpressionToString vm _ _ e@(RealVariableE v) = vm e
realExpressionToString _ _ _ (BoundVariableE) = "t"
realExpressionToString vm _ _ e@(Derivative (RealVariableE v)) = vm e
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

makeResiduals :: BasicDAEModel -> M.Map RealVariable Int -> M.Map RealCommonSubexpression String ->
                 M.Map BoolCommonSubexpression String -> String
makeResiduals m@(BasicDAEModel { equations = eqns, forcedInequalities = ieqs }) varNumMap realCSEMap boolCSEMap =
    let
        ieqStrList = map (inequalityTest (variableOrDerivDisplay varNumMap) realCSEMap
                          boolCSEMap) ieqs
        eqnStrList = map (equationToResidualString (variableOrDerivDisplay varNumMap) realCSEMap boolCSEMap) eqns
        strList = ieqStrList ++ eqnStrList
    in
      (concatMap makeResidual (zip [0..] eqnStrList)) ++
      if null ieqStrList then "" else
        "if (!(" ++ (intercalate "&&" ieqStrList) ++ "))\n\
         \  return 1;\n"

makeBoundaryResiduals :: BasicDAEModel -> M.Map RealExpression Int -> M.Map RealCommonSubexpression String ->
                 M.Map BoolCommonSubexpression String -> String
makeBoundaryResiduals m@(BasicDAEModel { equations = meqns, boundaryEquations = beqns, forcedInequalities = ieqs }) paramNumMap realCSEMap boolCSEMap =
    let
        eqns = (map ((,) (BoolConstant True)) meqns) ++ beqns
        estrList = map (\(c, eq) -> (boolExpressionToString (paramDisplay paramNumMap) realCSEMap boolCSEMap c,
                                    equationToResidualString (paramDisplay paramNumMap) realCSEMap boolCSEMap eq)) eqns
        ieqStrList = map (\ieq -> ("1", inequalityResidual (paramDisplay paramNumMap) realCSEMap boolCSEMap ieq)) ieqs
        strList = estrList ++ ieqStrList
    in
      concatMap (\(i, (c, res)) ->
                     if c == "1"
                     then
                         makeResidual (i, res)
                     else
                         "if (" ++ c ++ ")\n " ++ (makeResidual (i, res)) ++ "else\n " ++
                         (makeResidual (i, "0"))
                ) (zip [0..] strList)

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
    considerCSEIn = (equations mod, boundaryEquations mod, interventionRoots mod, forcedInequalities mod, checkedConditions mod)
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

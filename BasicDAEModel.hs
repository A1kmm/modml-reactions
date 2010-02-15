{-#LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable #-}
module BasicDAEModel
where

import Control.Monad.State
import Control.Monad.Identity
import Data.Data
import qualified Data.Map as M

class (Ord a) => Variable a
  where
    variableId :: a -> Int

newtype RealVariable = RealVariable Int deriving (Eq, Ord, Typeable, Data)
instance Variable RealVariable
  where
    variableId (RealVariable a) = a
instance Show RealVariable
    where
      showsPrec _ v = showString "Variable_" . shows (variableId v)

data RealEquation = RealEquation RealExpression RealExpression deriving (Eq, Ord, Typeable, Data)
data BoolEquation = BoolEquation BoolExpression BoolExpression deriving (Eq, Ord, Typeable, Data)

data BoolExpression =
    -- A constant true or false.
    BoolConstant Bool |
    -- A common subexpression tagged expression.
    BoolCommonSubexpressionE BoolCommonSubexpression |
    -- Logical and of two expressions.
    BoolExpression `And` BoolExpression |
    -- Logical not of an expression.
    Not BoolExpression |
    -- Logical or of two expressions.
    BoolExpression `Or` BoolExpression |
    RealExpression `LessThan` RealExpression |
    RealExpression `Equal` RealExpression
                   deriving (Eq, Ord, Typeable, Data)

data RealExpression =
    -- Any real value, as a constant.
    RealConstant Double |
    -- A free variable...
    RealVariableE RealVariable |
    -- The bound variable of the integration...
    BoundVariableE |
    -- The derivative of an expression with respect to the bound variable...
    Derivative RealExpression |
    -- A common subexpression tagged expression.
    RealCommonSubexpressionE RealCommonSubexpression |
    -- If x {- then -} b {- else -} b
    If BoolExpression RealExpression RealExpression |
    -- A sum of two expressions.
    RealExpression `Plus` RealExpression |
    -- First expression minus second expression.
    RealExpression `Minus` RealExpression |
    -- The product of two expressions.
    RealExpression `Times` RealExpression |
    -- a `Divided` {- by -} b
    RealExpression `Divided` RealExpression |
    -- a `Power` b - a to the power of b.
    RealExpression `Power` RealExpression |
    -- The floor function...
    Floor RealExpression |
    -- The ceiling function...
    Ceiling RealExpression |
    -- LogBase a b = log_a b
    LogBase RealExpression RealExpression |
    -- Trigonometric functions...
    Sin RealExpression |
    Tan RealExpression |
    Cos RealExpression |
    ASin RealExpression |
    ATan RealExpression |
    ACos RealExpression |
    Sinh RealExpression |
    Tanh RealExpression |
    Cosh RealExpression |
    ASinh RealExpression |
    ATanh RealExpression |
    ACosh RealExpression
          deriving (Eq, Ord, Typeable, Data)

class (Ord a) => CommonSubexpression a
  where
    commonSubexpressionId :: a -> Int

data RealCommonSubexpression = RealCommonSubexpression Int RealExpression deriving (Eq, Ord, Typeable, Data)
instance CommonSubexpression RealCommonSubexpression
  where
    commonSubexpressionId (RealCommonSubexpression a _) = a

data BoolCommonSubexpression = BoolCommonSubexpression Int BoolExpression deriving (Eq, Ord, Typeable, Data)
instance CommonSubexpression BoolCommonSubexpression
  where
    commonSubexpressionId (BoolCommonSubexpression a _) = a

data AnyCommonSubexpression = FromRealCommonSubexpression RealCommonSubexpression | FromBoolCommonSubexpression BoolCommonSubexpression deriving (Eq, Ord, Typeable, Data)
instance CommonSubexpression AnyCommonSubexpression
  where
    commonSubexpressionId (FromRealCommonSubexpression r) = commonSubexpressionId r
    commonSubexpressionId (FromBoolCommonSubexpression b) = commonSubexpressionId b

data BasicDAEModel = BasicDAEModel {
        -- The equations which apply for this model.
        equations :: [RealEquation],
        -- Expressions which controls when the solver needs to be restarted.
        -- The values should cross zero at the point when the solver needs to
        -- be restarted.
        interventionRoots :: [RealExpression],
        -- An expression which the solver should try to keep positive.
        forcedInequalities :: [RealExpression],
        checkedConditions :: [(String, BoolExpression)],
        variables :: [RealVariable],
        commonSubexpressions :: [AnyCommonSubexpression],
        annotations :: M.Map (String, String) String,
        nextID :: Int
    } deriving (Eq, Ord, Typeable, Data)
nullModel = BasicDAEModel { equations = [], interventionRoots = [], forcedInequalities = [], checkedConditions = [],
                            variables = [], commonSubexpressions = [], annotations = M.empty, nextID = 0
                          }

type ModelBuilderT m a = StateT BasicDAEModel m a
type ModelBuilder a = ModelBuilderT Identity a

buildModelT x = execStateT x nullModel
buildModel = runIdentity . buildModelT

x `newEqM` y = modify (\m -> m { equations = (RealEquation x y):(equations m) })
x `newEqX` y = do
  x' <- x
  y' <- y
  x' `newEqM` y'
newEq = newEqX

newInterventionRootM x = modify (\m -> m { interventionRoots = x:(interventionRoots m)})
newInterventionRootX x =
    do
      x' <- x
      newInterventionRootM x'
newInterventionRoot = newInterventionRootX

newForcedInequalityM x = modify (\m -> m { forcedInequalities = x:(forcedInequalities m)})
newForcedInequalityX x =
    do
      x' <- x
      newForcedInequalityM x'
newForcedInequality = newForcedInequalityX

msg `newCheckedConditionM` x = modify (\m -> m { checkedConditions = (msg, x):(checkedConditions m) })
m `newCheckedConditionX` x = do
  x' <- x
  m `newCheckedConditionM` x'
newCheckedCondition = newCheckedConditionX

annotateModel :: (Show a, Show b, Show c, Monad m) => a -> b -> c -> ModelBuilderT m ()
annotateModel s p o = modify (\m -> m { annotations = M.insert ((show s), (show p)) (show o) (annotations m) })

registerCommonSubexpression s =
    modify (\m -> m { commonSubexpressions = s:(commonSubexpressions m)})

allocateID :: Monad m => ModelBuilderT m Int
allocateID = modify (\m -> m { nextID = (+1) $ nextID m}) >> (gets $ flip (-) 1 . nextID)

boolConstantM :: Monad m => Bool -> ModelBuilderT m BoolExpression
boolConstantM = return . BoolConstant

trueM :: Monad m => ModelBuilderT m BoolExpression
trueM = boolConstantM True
falseM :: Monad m => ModelBuilderT m BoolExpression
falseM = boolConstantM False

boolCommonSubexpressionM :: Monad m => BoolExpression -> ModelBuilderT m BoolExpression
boolCommonSubexpressionM e =
  do
    id <- allocateID
    let bcs = BoolCommonSubexpression id e
    registerCommonSubexpression (FromBoolCommonSubexpression bcs)
    return $ BoolCommonSubexpressionE bcs

boolCommonSubexpression me = me >>= boolCommonSubexpressionM
boolCommonSubexpressionX = boolCommonSubexpression

andM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
a `andM` b = return $ a `And` b
(.&&.) :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
(.&&.) = liftM2 And
andX = (.&&.)

orM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
a `orM` b = return $ a `Or` b
(.||.) :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
(.||.) = liftM2 Or
orX = (.||.)

notM :: Monad m => BoolExpression -> ModelBuilderT m BoolExpression
notM = return . Not
notX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
notX = liftM Not

lessThanM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `lessThanM` b = return $ a `LessThan` b
lessThanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `lessThanX` b = do
  a' <- a
  b' <- b
  a' `lessThanM` b'
(.<.) = lessThanX

equalM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `equalM` b = return $ a `Equal` b
equalX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `equalX` b = do
  a' <- a
  b' <- b
  a' `equalM` b'
(.==.) = equalX

realConstantM :: Monad m => Double -> ModelBuilderT m RealExpression
realConstantM = return . RealConstant
realConstant = realConstantM

realCommonSubexpressionM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
realCommonSubexpressionM e =
  do
    id <- allocateID
    let rcs = RealCommonSubexpression id e
    registerCommonSubexpression (FromRealCommonSubexpression rcs)
    return $ RealCommonSubexpressionE rcs
realCommonSubexpression me = me >>= realCommonSubexpressionM
realCommonSubexpressionX = realCommonSubexpression

mkNewRealVariable :: Monad m => ModelBuilderT m RealVariable
mkNewRealVariable = do
  id <- allocateID
  let v = RealVariable id
  modify (\m -> m { variables = v:(variables m) } )
  return v

mkNewRealVariableM :: Monad m => ModelBuilderT m (ModelBuilderT m RealVariable)
mkNewRealVariableM = liftM return mkNewRealVariable

realVariableM :: Monad m => RealVariable -> ModelBuilderT m RealExpression
realVariableM = return . RealVariableE 
realVariableX :: Monad m => ModelBuilderT m RealVariable -> ModelBuilderT m RealExpression
realVariableX = liftM RealVariableE
realVariable = realVariableX

newRealVariableE = realVariable (mkNewRealVariable)
newRealVariable :: Monad m => ModelBuilderT m (ModelBuilderT m RealExpression)
newRealVariable = do
  v <- newRealVariableE
  return (return v)

boundVariableM :: Monad m => ModelBuilderT m RealExpression
boundVariableM = return $ BoundVariableE
boundVariableX = boundVariableM
boundVariable = boundVariableM

derivativeM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
derivativeM = return . Derivative
derivativeX = liftM Derivative
derivative = derivativeX

ifM :: Monad m => BoolExpression -> RealExpression ->
                      RealExpression -> ModelBuilderT m RealExpression
ifM c {- then -} e1 {- else -} e2 = return $ If c e1 e2
ifX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m RealExpression ->
                     ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
ifX = liftM3 If

plusM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `plusM` b = return $ a `Plus` b
(.+.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.+.) = liftM2 Plus
plusX = (.+.)

minusM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `minusM` b = return $ a `Minus` b
(.-.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.-.) = liftM2 Minus
minusX = (.-.)

timesM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `timesM` b = return $ a `Times` b
(.*.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.*.) = liftM2 Times
timesX = (.*.)

dividedM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `dividedM` b = return $ a `Divided` b
(./.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(./.) = liftM2 Divided
dividedX = (./.)

powerM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `powerM` b = return $ a `Power` b
(.**.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.**.) = liftM2 Power
powerX = (.**.)

logBaseM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `logBaseM` b = return $ a `LogBase` b
logBaseX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
logBaseX = liftM2 LogBase

floorM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
floorM = return . Floor
floorX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
floorX = liftM Floor
ceilingM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
ceilingM = return . Ceiling
ceilingX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
ceilingX = liftM Floor
sinM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sinM = return . Sin
sinX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
sinX = liftM Sin
tanM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
tanM = return . Tan
tanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
tanX = liftM Tan
cosM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
cosM = return . Cos
cosX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
cosX = liftM Cos
asinM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
asinM = return . ASin
asinX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
asinX = liftM ASin
atanM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
atanM = return . ATan
atanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
atanX = liftM ATan
acosM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
acosM = return . ACos
acosX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
acosX = liftM ACos
sinhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sinhM = return . Sinh
sinhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
sinhX = liftM Sinh
tanhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
tanhM = return . Tanh
tanhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
tanhX = liftM Tanh
coshM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
coshM = return . Cosh
coshX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
coshX = liftM Cosh
asinhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
asinhM = return . ASinh
asinhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
asinhX = liftM ASinh
atanhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
atanhM = return . ATanh
atanhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
atanhX = liftM ATanh
acoshM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
acoshM = return . ACosh
acoshX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
acoshX = liftM ACosh

-- Now define some constants...

-- The constant pi.
piM :: Monad m => ModelBuilderT m RealExpression
piM = realConstantM pi
piX = piM

-- The constant e.
econstantE = RealConstant (exp 1.0)
econstant :: Monad m => ModelBuilderT m RealExpression
econstant = realConstantM (exp 1.0)
econstantM = econstant
econstantX = econstant

-- Now some functions which are simple applications of existing functions

-- The exp function, made using .**.
expE = (econstantE `Power`)
expM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
expM = return . expE
expX m = m >>= expM

-- The negative of the expression.
negateE = ((RealConstant (-1.0)) `Times`)
negateM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
negateM = return . negateE
negateX m = m >>= negateM

-- The square root of an expression.
sqrtE x = x `Power` (RealConstant 0.5)
sqrtM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sqrtM = return . sqrtE
sqrtX m = m >>= sqrtM

-- The log base e of an expression.
logE = LogBase econstantE
logM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
logM x = return $ logE x
logX m = m >>= logM

-- Now some more complex functions that only have M and X forms...
boolIfM :: Monad m => BoolExpression -> BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
boolIfM cond x1 x2 =
    do
      condc <- boolCommonSubexpressionM cond
      (condc `andM` x1) .||. ((Not condc) `andM` x2)
boolIfX mcond mx1 mx2 =
    do
      cond <- mcond
      x1 <- mx1
      x2 <- mx2
      boolIfM

xorM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
xorM x1 x2 =
    do
      x1c <- boolCommonSubexpressionM x1
      (x1c `andM` (Not x2)) .||. ((Not x1c) `andM` x2)
xorX mx1 mx2 =
    do
      x1 <- mx1
      x2 <- mx2
      xorM x1 x2


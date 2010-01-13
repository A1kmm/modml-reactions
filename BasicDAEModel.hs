module BasicDAEModel
where

import Control.Monad.State
import Control.Monad.Identity

class (Ord a) => Variable a
  where
    variableId :: a -> Int

newtype RealVariable = RealVariable Int deriving (Eq, Ord)
instance Variable RealVariable
  where
    variableId (RealVariable a) = a

newtype BoolVariable = BoolVariable Int deriving (Eq, Ord)
instance Variable BoolVariable
  where
    variableId (BoolVariable a) = a

data AnyVariable = FromRealVariable RealVariable | FromBoolVariable BoolVariable
instance Variable AnyVariable
  where
    variableId (FromRealVariable r) = variableId r
    variableId (FromBoolVariable b) = variableId b
instance Eq AnyVariable
  where
    a==b = (variableId a) == (variableId b)
instance Ord AnyVariable
  where
    a<b = (variableId a) < (variableId b)

data RealEquation = RealEquation RealExpression RealExpression
data BoolEquation = BoolEquation BoolExpression BoolExpression

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
    BoolExpression `Or` BoolExpression
                   deriving (Eq, Ord)

(.&&.) = And
(.||.) = Or

data RealExpression =
    -- Any real value, as a constant.
    RealConstant Double |
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
          deriving (Eq, Ord)

class (Ord a) => CommonSubexpression a
  where
    commonSubexpressionId :: a -> Int

data RealCommonSubexpression = RealCommonSubexpression Int RealExpression deriving (Eq, Ord)
instance CommonSubexpression RealCommonSubexpression
  where
    commonSubexpressionId (RealCommonSubexpression a _) = a

data BoolCommonSubexpression = BoolCommonSubexpression Int BoolExpression deriving (Eq, Ord)
instance CommonSubexpression BoolCommonSubexpression
  where
    commonSubexpressionId (BoolCommonSubexpression a _) = a

data AnyCommonSubexpression = FromRealCommonSubexpression RealCommonSubexpression | FromBoolCommonSubexpression BoolCommonSubexpression deriving (Eq, Ord)
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
        checkedConditions :: [(String, RealExpression)],
        variables :: [AnyVariable],
        commonSubexpressions :: [AnyCommonSubexpression]
    }
nullModel = BasicDAEModel { equations = [], interventionRoots = [], forcedInequalities = [], checkedConditions = [],
                            variables = [], commonSubexpressions = []
                          }
mergeModels m1 m2 = BasicDAEModel {
                      equations = (equations m1) ++ (equations m2),
                      interventionRoots = (interventionRoots m1) ++ (interventionRoots m2),
                      forcedInequalities = (forcedInequalities m1) ++ (forcedInequalities m2),
                      checkedConditions = (checkedConditions m1) ++ (checkedConditions m2),
                      variables = (variables m1) ++ (variables m2),
                      commonSubexpressions = (commonSubexpressions m1) ++ (commonSubexpressions m2)
                    }

type ModelBuilderT m a = StateT BasicDAEModel m a
type ModelBuilder a = ModelBuilderT Identity a

instance MonadPlus ModelBuilder
  where
    mplus = liftM2 mergeModels

buildModelT = evalStateT nullModel
buildModel = runIdentity . buildModelT

{-
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
    BoolExpression `Or` BoolExpression
                   deriving (Eq, Ord)

(.&&.) = And
(.||.) = Or
-}

registerCommonSubexpression s =
    modify (\m -> m { commonSubexpressions = s:(commonSubexpressions m)})
      

boolConstantM :: Bool -> ModelBuilder BoolExpression
boolConstantM = return . BoolConstant



(.+.) = Plus
(.-.) = Minus
(.*.) = Times
(.**.) = Power

-- The constant pi.
piE = RealConstant pi
-- The constant e.
econstantE = RealConstant (exp 1.0)

-- The exp function, made using .**.
expE = (econstantE .**.)

-- The negative of the expression.
negateE = ((RealConstant (-1.0)) `Times`)

-- The square root of an expression.
sqrtE = flip (.**.) (RealConstant 0.5)

-- The log base e of an expression.
logE = LogBase econstantE

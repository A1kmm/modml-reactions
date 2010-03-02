import BasicDAEModel
import BasicDAESolver

mymodelBuilder = do
  x <- newRealVariable
  y <- newRealVariable
  initialValue {- at time -} 0 {-, -} x {- == -} 10
  initialValue {- at time -} 0 {-, -} (derivative x) {- == -} 10
  (derivative (derivative x)) `newEq` (realConstant 10)
  -- (derivative (x .**. (realConstant 2))) `newEq` (realConstant 10)
  -- initialValue 0 y (-5)
  -- initialValue 0 (derivative y) 0
  (y .+. x) `newEq` (realConstant 5)
  -- newForcedInequality ((realConstant 100) .-. x)
  -- newCheckedCondition "X out of range" ((realConstant 100) .<. x)
  newInterventionRoot (x.-.(realConstant 100))

mymodel = buildModel mymodelBuilder
main = print $ modelToResults mymodel defaultSolverParameters

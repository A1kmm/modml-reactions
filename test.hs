import BasicDAEModel
import BasicDAESolver

mymodelBuilder = do
  x <- newRealVariable
  y <- newRealVariable
  initialValue {- at time -} 0 {-, -} x {- == -} 10
  initialValue {- at time -} 0 {-, -} (derivative x) {- == -} 10
  (derivative (derivative x)) `newEq` (realConstant 10)
  -- (derivative (x .**. (realConstant 2))) `newEq` (realConstant 10)
  initialValue 0 y (-5)
  initialValue 0 (derivative y) 0
  (y .+. x) `newEq` (realConstant 5)
  {- newForcedInequality (x)
  newCheckedCondition "X out of range" ((realConstant 10) .<.)
  newInterventionRoot (x.-.(realConstant 10)) -}

mymodel = buildModel mymodelBuilder
showCode (Left err) = show err
showCode (Right code) = showString code "\n"
main = putStr $ showCode $ makeCodeFor mymodel
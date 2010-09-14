{-# LANGUAGE DeriveDataTypeable #-}

{- |
  The ModML reactions module used to describe a system in terms of processes on
  entities.
 -}
module ModML.Reactions.Reactions
where

import qualified Data.Set as S
import Data.Set ((\\))
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Data as D
import qualified Data.TypeHash as D
import qualified ModML.Units.UnitsDAEModel as U
import qualified ModML.Core.BasicDAEModel as C
import Control.Monad
import Data.Maybe
import Data.Generics
import Data.List hiding ((\\))

{- | Entity represents a specific 'thing' for which some sense of quantity can
     be measured by a single real number. An entity can be an abstract
     concept like energy, or be more specific, like a particular molecule.
 -}
data Entity = Entity U.Units Int deriving (Eq, Ord, D.Typeable, D.Data)

{- | Compartment describes a place at which an entity can be located, and measured.
     The 'place' can be either physical or conceptual, depending on the
     requirements of the model.
 -}
newtype Compartment = Compartment Int deriving (Eq, Ord, D.Typeable, D.Data)

{- | Amount describes how much of an entity there is in a particular compartment,
     for a particular value of the independent variable. It may be described in
     terms of concentration, or molar amounts, or charge, or charge per area or
     volume, depending on the entity and the requirements of the model.
 -}
newtype Amount = Amount Double U.Units

-- |  CompartmentEntity describes a specific entity, in a specific compartment.
type CompartmentEntity = (Entity, Compartment)

{- | Process describes a transformation that can occur which depends on, and changes,
     the amount of one or more EntityCompartments. Processes describe what can
     happen, not what does happen (for example, a process may only happen in the
     presence of a particular enzyme - but the process can still be defined, even
     in a compartment where that enzyme is not present, in which case it won't
     affect the results).
 -}
data Process = Process {
      {- | activationCriterion is used to eliminate processes which do not apply
           because the CompartmentEntity essential for the process is missing. It
           is intended as an optimisation only - if we can't prove that a given
           CompartmentEntity is always zero, the process will still be used, even
           if the CompartmentEntity is actually always zero.
       -}
      activationCriterion :: (S.Set CompartmentEntity) -> Bool,
      {- | CompartmentEntities which this process can potentially
           produce even when the initial amount of that CompartmentEntity
           is zero (for example, reaction products, and reactants of a
           reversible process).
       -}
      creatableCompartmentEntities :: S.Set CompartmentEntity,
      {- | All CompartmentEntities for which this process can change the amount.
           This includes species from both the left and right hand side of a reaction.
       -}
      modifiableCompartmentEntities :: S.Set CompartmentEntity,
      {- | A mapping from placeholder variables to the ComponentEntity
           amount they represent.
       -}
      entityVariables :: M.Map U.RealVariable CompartmentEntity,
      {- | The stoichiometry of each CompartmentEntity; this consists of a
           multiplier to be applied for each CompartmentEntity when computing
           the flux, and the units of that multiplier. It should be negative for
           reactants, and positive for products.
       -}
      stoichiometry :: M.Map CompartmentEntity (Double, U.Units),
      {- | A template for the process rate. Fluxes are generated from this as follows:
           * Placeholders in entityVariables will be replaced.
           * The multiplier specified in the stoichiometry is applied.
       -}
      rateTemplate :: U.RealExpression
    }  deriving (D.Typeable, D.Data)

-- | EntityInstance describes how an EntityCompartment fits into the model.
data EntityInstance =
    {- | The entity amount is known, but the reactions occurring don't
         change it. This can be used for several purposes:
         * The amount can be fixed as a constant zero. This is detected, and
           used to simplify the model by removing species which don't matter.
         * The amount can be fixed if changes over time are so negligible that
           it is simpler to fix the level.
         * The value being clamped to could be a variable from a different part
           of the model.
    -}
    EntityClamped U.RealExpression |
    {- | The entity amount starts at an initial value, and changes with time as
         a result of the processes acting on it. The second field stores the expression
         to be added to the fluxes to make the final rate of change.
     -}
    EntityFromProcesses U.RealExpression U.RealExpression

-- | A ReactionModel describes which processes occur, and where.
data ReactionModel = ReactionModel {
      {- | The list of processes where the compartment is known (for example,
           a Process which only occurs in one Compartment).
       -}
      explicitCompartmentProcesses :: [Process],

      {- | Functions which should be used to generate a Process for every Compartment
           in the model
       -}
      allCompartmentProcesses :: [Compartment -> Process],

      {- | Functions for generating a Process from every pair of Compartments
           where one compartment is contained in another. Useful for processes
           which occur across membranes, for example.
       -}
      containedCompartmentProcesses :: [(Compartment, Compartment) -> Process],

      -- | A description of which Compartments are contained within which other compartments.
      containment :: S.Set (Compartment, Compartment),

      {- | A map from each compartment entity to the description of the
           entity which contains it.
       -}
      entityInstances :: M.Map CompartmentEntity EntityInstance,

      -- | Annotations on the reaction model...
      annotations :: M.Map (String, String) String,

      -- | Context tagged IDs, tagged to a metatype and a tag type.
      contextTaggedIDs :: M.Map (D.TypeCode, D.TypeCode) Int,

      -- | The next identifier to allocate.
      nextID :: Int
    } deriving (D.Typeable, D.Data)

-- From here on in, we describe the algorithm to convert from a reaction to a units DAE model.

data ProcessActivation = ProcessActivation {
      activeProcesses :: S.Set Process,
      newActiveProcesses :: [Process],
      candidateProcesses :: S.Set Process,
      activeCompartmentEntities :: S.Set CompartmentEntity,
      newActiveCompartmentEntities :: [CompartmentEntity],
      definiteInactiveCompartmentEntities :: S.Set CompartmentEntity,
      processedCompartments :: S.Set Compartment
    }

data ThreeState = DefiniteYes | Possibly | DefiniteNo

splitFstBySnd3State :: [(a, ThreeState)] -> ([a], [a], [a])
splitFstBySnd3State [] = []
splitFstBySnd3State ((a, st3):b) =
    let
        (y, p, n) = splitFirstBySecond b
    in
      case st3
      of
        DefiniteYes -> (a:y, p, n)
        Possibly -> (y, a:p, n)
        DefiniteNo -> (y, p, a:n)
withSnd :: (b -> c) -> (a, b) -> (a, c)
withSnd f (a, b) = (a, f b)

isExpressionNonzero :: U.RealExpression -> ThreeState
isExpressionNonzero ex =
    let
        mc = tryEvaluateRealAsConstant . snd . U.translateRealExpression $ ex
    in
      case mc
      of
        Just c | c == 0 -> DefiniteNo
               | otherwise -> DefiniteYes
        Nothing -> Possibly

isEntityInstanceNonzero :: EntityInstance -> ThreeState
isEntityInstanceNonzero (EntityClamped ex)= isExpressionNonzero ex
isEntityInstanceNonzero (EntityFromProcesses v0ex dvex) =
    case (isExpressionNonzero v0ex, isExpressionNonzero dvex)
    of
      -- If either the initial value or rate are non-zero, treat as non-zero...
      (DefiniteYes, _) -> DefiniteYes
      (_, DefiniteYes) -> DefiniteYes
      -- Otherwise it depends on the processes activated...
      _ -> Possibly

startingProcessActivation m =
    let
        (definiteCEs, _, definitelyNotCEs) =
            splitFstBySnd3State $ map (withSnd isEntityInstanceNonzero) (M.toList . entityInstances $ m)
    in
      ProcessActivation { activeProcesses = S.empty,
                          newActiveProcesses = [],
                          candidateProcesses = S.fromList . explicitCompartmentProcesses $ m,
                          activeCompartmentEntities = S.fromList definiteCEs,
                          newActiveCompartmentEntities = definiteCEs,
                          definiteInactiveCompartmentEntities = S.fromList definitelyNotCEs,
                          madeProgress = True
                        }

findMissingCompartments pa =
    (S.fromList $ map snd $ newActiveCompartmentEntities pa) \\ (processedCompartments pa)

newCompartmentsToNewProcesses :: ReactionModel -> [Compartment] -> [Process]
newCompartmentsToNewProcesses m newcs =
    flip concatMap newcs $ \comp ->
        map (flip id comp) allCompartmentProcesses

doProcessActivation :: ReactionModel -> ProcessActivation -> ProcessActivation
doProcessActivation m pa =
    let
        newCompartments = findMissingCompartments m pa
        candprocs = (candidateProcesses pa) `S.union` (newCompartmentsToNewProcesses m newCompartments)
        newActivePs = S.filter (flip activationCriterion (activeCompartmentEntities pa)) candprocs
        newActiveCEs = S.unions $ map creatableCompartmentEntities
                                    (S.toList newActivePs)
    in
      ProcessActivation { activeProcesses = S.union (activeProcesses pa) newActivePs,
                          newActiveProcesses = S.toList newActivePs,
                          candidateProcesses = candprocs \\ newActivePs,
                          activeCompartmentEntities =
                              (activeCompartmentEntities pa) `S.union`
                              (S.fromList (newActiveCEs \\ 
                                           (definiteInactiveCompartmentEntities pa))),
                          newActiveCompartmentEntities = newActiveCEs,
                          definiteInactiveCompartmentEntities = (definiteInactiveCompartmentEntities pa),
                          processedCompartments = S.union (processedCompartments pa) (S.fromList newCompartments)
                        }
    
noMoreProcessActivation pa = null (newActiveProcesses pa) && null (newActiveCompartmentEntities pa)

reactionModelToUnits :: Monad m => ReactionModel -> U.ModelBuilderT m ()
reactionModelToUnits m = do
    let activeCEs = activeCompartmentEntities $
                      until noMoreProcessActivation
                        (doProcessActivation m) (startingProcessActivation m)
    let processes = (explicitCompartmentProcesses m) ++
                      (concatMap (\c -> map (flip uncurry c) (containedCompartmentProcesses m))
                                 ((S.toList . containment) m))
    let eis = flip map activeCEs $ \ce@(_, Entity u _) ->
                case M.lookup ce (entityInstances m)
                of
                  Just ei -> (ce, ei)
                  Nothing -> (ce, EntityFromProcess (U.realConstantE u 0) (U.realConstantE u 0))
    let eiMap = M.fromList eis
    -- Each active CompartmentEntity gets a corresponding variable in the model...
    ceToVar <-
      liftM M.fromList $ forM activeCEs $ \ce@(_, Entity u _) -> do
        v <- mkNewRealVariable u
        -- To do: annotate model so CE can be identified.
        return (ce, v)
    -- and each process has a rate variable and a rate equation...
    procToRateVar <-
        liftM M.fromList $ forM processes $ \p -> do
          v <- mkNewRealVariable U.dimensionlessE
          (U.RealVariableE v) `U.newEqM` (substituteRateTemplate ceToVar p)
          return (p, v)
    let fluxMap = buildFluxMap processes procToRateVar ceToVar
    -- Each CE / EntityInstance also has an equation...
    forM eis $ \(ce, ei) -> do
      let v = fromJust $ M.lookup ce ceToVar
      case ei
        of
          EntityClamped ex -> (U.RealVariableE v) `U.newEqM` ex
          EntityFromProcesses ivex rateex -> do
              (U.RealVariableE v) `U.newEqM` ivex
              let rate = case M.lookup ce fluxMap
                           of
                             Nothing -> rateex
                             Just fluxes -> fluxes `U.plusM` rateex
              (U.Derivative (U.RealVariableE v)) `U.newEqM` rate

flipPair (a, b) = (b, a)
transposeMap = M.fromList . map flipPair . M.toList
composeMaps :: M.Map k a -> M.Map a b -> M.Map k b
composeMaps m1 m2 = M.mapMaybe (flip M.lookup m2) m1
substituteRateTemplate ceToVar p =
  let
      varSub = composeMaps (entityVariables p) ceToVar
  in
    everywhere (mkT $ substituteOneVariable varSub) (rateTemplate p)

substituteOneVariable :: M.Map U.RealVariable U.RealVariable -> U.RealExpression -> U.RealExpression
substituteOneVariable varSub (U.RealVariableE v) =
    case (M.lookup v varSub)
      of
        Just v' -> v'
        Nothing -> U.RealVariableE v
substituteOneVariable _ ex = ex

forFoldl' s0 l f = foldl' f s0 l
alterWith m k f = M.alter f k m

buildFluxMap :: [Process] -> M.Map Process U.RealVariable -> M.Map CompartmentEntity U.RealVariable -> M.Map CompartmentEntity U.RealExpression
buildFluxMap processes procToVar ceToVar = buildFluxMap' processes procToVar ceToVar M.empty
buildFluxMap' [] _ _ m = m
buildFluxMap' (p:processes) procToVar ceToVar m =
    let
        procVar = procToVar!p
        m' = forFoldl' m (M.toList $ stoichiometry p) $ \m0 (ce, (mup, u)) ->
             case (M.lookup ce ceToVar)
               of
                 Nothing -> m0
                 Just ceVar ->
                     let rhs = (U.RealVariableE ceVar) `U.Times` (U.realConstantE u mup)
                       in
                         alterWith m0 ce $ \re0 ->
                             case re0
                             of
                               Nothing -> rhs
                               Just re -> re `U.Plus` rhs
    in
      buildFluxMap' processes procToVar ceToVar m'

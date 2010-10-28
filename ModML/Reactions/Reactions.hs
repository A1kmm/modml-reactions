{-# LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable,MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,TemplateHaskell #-}

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
import qualified ModML.Core.BasicDAEModel as B
import qualified ModML.Core.BasicDAEModel as C
import qualified ModML.Units.SIUnits as SI
import Control.Monad
import qualified Control.Monad.Trans as M
import Data.Maybe
import Data.Generics
import Data.List hiding ((\\))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Language.Haskell.TH.Syntax as T
import qualified Data.Char as C
import ModML.Units.UnitsDAEOpAliases

{- | Entity represents a specific 'thing' for which some sense of quantity can
     be measured by a single real number. An entity can be an abstract
     concept like energy, or be more specific, like a particular molecule.
 -}
data Entity = Entity U.Units Int deriving (Eq, Ord, Show, D.Typeable, D.Data)

{- | Compartment describes a place at which an entity can be located, and measured.
     The 'place' can be either physical or conceptual, depending on the
     requirements of the model.
 -}
newtype Compartment = Compartment Int deriving (Eq, Ord, Show, D.Typeable, D.Data)

{- | Amount describes how much of an entity there is in a particular compartment,
     for a particular value of the independent variable. It may be described in
     terms of concentration, or molar amounts, or charge, or charge per area or
     volume, depending on the entity and the requirements of the model.
 -}
data Amount = Amount Double U.Units deriving (Show)

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
      -- | The unique identifier for the Process.
      processId :: Int,

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
instance Eq Process where
    Process { processId = ida} == Process { processId = idb } = ida == idb
instance Ord Process where
    Process { processId = ida} `compare` Process { processId = idb } = ida `compare` idb

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
    deriving (D.Typeable, D.Data)
-- | A ReactionModel describes which processes occur, and where.
data ReactionModel = ReactionModel {
      {- | The list of processes where the compartment is known (for example,
           a Process which only occurs in one Compartment).
       -}
      explicitCompartmentProcesses :: [Process],

      {- | Functions which should be used to generate a Process for every Compartment
           in the model
       -}
      allCompartmentProcesses :: [Int -> Compartment -> Process],

      {- | Functions for generating a Process from every pair of Compartments
           where one compartment is contained in another. Useful for processes
           which occur across membranes, for example.
       -}
      containedCompartmentProcesses :: [Int -> (Compartment, Compartment) -> Process],

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

entityClamped :: Monad m => U.ModelBuilderT m U.RealExpression -> ModelBuilderT m EntityInstance
entityClamped ex = do
  ex' <- U.liftUnits ex
  return $ EntityClamped ex'

entityFromProcesses :: Monad m => U.ModelBuilderT m U.RealExpression -> U.ModelBuilderT m U.RealExpression -> ModelBuilderT m EntityInstance
entityFromProcesses iv flux = do
  iv' <- U.liftUnits iv
  flux' <- U.liftUnits flux
  return $ EntityFromProcesses iv' flux'

emptyReactionModel = ReactionModel {
                       explicitCompartmentProcesses = [],
                       allCompartmentProcesses = [],
                       containedCompartmentProcesses = [],
                       containment = S.empty,
                       entityInstances = M.empty,
                       annotations = M.empty,
                       contextTaggedIDs = M.empty,
                       nextID = 0
                     }

-- | The independent variable units for a reaction model, which is always seconds.
independentUnits = SI.uSecond

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
splitFstBySnd3State [] = ([], [], [])
splitFstBySnd3State ((a, st3):b) =
    let
        (y, p, n) = splitFstBySnd3State b
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
        mc = B.tryEvaluateRealAsConstant . snd . B.evalModel . U.runInCore independentUnits .
             U.translateRealExpression $ ex
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
        (m', containProcs) = foldl' (\s (comp1, comp2) ->
                                       foldl' (\(m'', l) f ->
                                               (m''{nextID = nextID m'' + 1},
                                                (f (nextID m'') (comp1, comp2)):l)
                                               ) s (containedCompartmentProcesses m)
                                    ) (m, []) (S.toList $ containment m)
    in
      (m', ProcessActivation { activeProcesses = S.empty,
                               processedCompartments = S.empty,
                               newActiveProcesses = [],
                               candidateProcesses = S.fromList . explicitCompartmentProcesses $ m,
                               activeCompartmentEntities = S.fromList definiteCEs,
                               newActiveCompartmentEntities = definiteCEs,
                               definiteInactiveCompartmentEntities = S.fromList definitelyNotCEs
                             })

findMissingCompartments pa =
    (S.fromList $ map snd $ newActiveCompartmentEntities pa) \\ (processedCompartments pa)

newCompartmentsToNewProcesses :: ReactionModel -> [Compartment] -> (ReactionModel, [Process])
newCompartmentsToNewProcesses m newcs = 
    foldl' (\s comp ->
                foldl' (\(m', l) f -> (m'{ nextID = nextID m' + 1},
                                       (f (nextID m') comp):l)) s (allCompartmentProcesses m)
            ) (m, []) newcs

doProcessActivation :: (ReactionModel, ProcessActivation) -> (ReactionModel, ProcessActivation)
doProcessActivation (m, pa) =
    let
        newCompartments = findMissingCompartments pa
        (m', newProcs) = newCompartmentsToNewProcesses m (S.toList newCompartments)
        candprocs = (candidateProcesses pa) `S.union` (S.fromList newProcs)
        newActivePs = S.filter (not . flip activationCriterion (activeCompartmentEntities pa)) candprocs
        newActiveCEs = S.unions $ map creatableCompartmentEntities
                         (S.toList newActivePs)
    in
      (m',
       ProcessActivation { activeProcesses = S.union (activeProcesses pa) newActivePs,
                           newActiveProcesses = S.toList newActivePs,
                           candidateProcesses = candprocs \\ newActivePs,
                           activeCompartmentEntities =
                               (activeCompartmentEntities pa) `S.union`
                               (newActiveCEs \\ 
                                            (definiteInactiveCompartmentEntities pa)),
                           newActiveCompartmentEntities = S.toList newActiveCEs,
                           definiteInactiveCompartmentEntities = (definiteInactiveCompartmentEntities pa),
                           processedCompartments = S.union (processedCompartments pa) (newCompartments)
                         }
      )
    
noMoreProcessActivation (_, pa) = null (newActiveProcesses pa) && null (newActiveCompartmentEntities pa)

reactionModelToUnits :: Monad m => ReactionModel -> U.ModelBuilderT m (M.Map CompartmentEntity U.RealVariable, M.Map Process U.RealVariable)
reactionModelToUnits m = do
    let (m', activation) = until noMoreProcessActivation
                             doProcessActivation (startingProcessActivation m)
    let processes = S.toList . activeProcesses $ activation
    let activeCEs = activeCompartmentEntities activation
    perTime <- U.boundUnits $**$ (-1)
    let eis = flip map (S.toList activeCEs) $ \ce@(Entity u _, _) ->
                case M.lookup ce (entityInstances m)
                of
                  Just ei -> (ce, ei)
                  Nothing -> (ce, EntityFromProcesses (U.realConstantE u 0)
                                                      (U.realConstantE (u `U.unitsTimes` perTime) 0))
    let eiMap = M.fromList eis
    -- Each active CompartmentEntity gets a corresponding variable in the model...
    ceToVar <-
      liftM M.fromList $ forM (S.toList activeCEs) $ \ce@(e@(Entity u eid), c@(Compartment cid)) -> do
        -- Get the name of the entity...
        let entityName = fromMaybe (shows eid "Unnamed Entity") . liftM read $ getAnnotationFromModel m e "nameIs"
        let compartmentName = fromMaybe (shows cid "Unnamed Compartment") . liftM read $ getAnnotationFromModel m c "nameIs"
        let vname = showString "Amount of " . showString entityName . showString " in " $ compartmentName
        v <- U.newNamedRealVariable (return u) vname
        return (ce, v)
    -- and each process has a rate variable and a rate equation...
    procToRateVar <-
        liftM M.fromList $ forM processes $ \p -> do
          v <- U.newRealVariable (U.boundUnits $**$ (-1))
          (U.RealVariableE v) `U.newEqM` (substituteRateTemplate ceToVar p)
          return (p, v)
    let fluxMap = buildFluxMap processes procToRateVar
    -- Each CE / EntityInstance also has an equation...
    forM_ eis $ \(ce, ei) -> do
      let v = fromJust $ M.lookup ce ceToVar
      case ei
        of
          EntityClamped ex -> (U.RealVariableE v) `U.newEqM` ex
          EntityFromProcesses ivex rateex -> do
              U.newBoundaryEq (U.realConstant U.boundUnits 0 .==. U.boundVariable) (U.realVariableM v) (return ivex)
              let rate = case M.lookup ce fluxMap
                           of
                             Nothing -> rateex
                             Just fluxes -> fluxes `U.Plus` rateex
              (U.Derivative (U.RealVariableE v)) `U.newEqM` rate
    return (ceToVar, procToRateVar)

flipPair (a, b) = (b, a)

transposeMap :: (Ord k, Ord a) => M.Map k a -> M.Map a k
transposeMap = M.fromList . map flipPair . M.toList
composeMaps :: (Ord k, Ord a) => M.Map k a -> M.Map a b -> M.Map k b
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
        Just v' -> U.RealVariableE v'
        Nothing -> U.RealVariableE v
substituteOneVariable _ ex = ex

forFoldl' s0 l f = foldl' f s0 l
alterWith m k f = M.alter f k m

buildFluxMap :: [Process] -> M.Map Process U.RealVariable -> M.Map CompartmentEntity U.RealExpression
buildFluxMap processes procToVar = buildFluxMap' processes procToVar M.empty
buildFluxMap' [] _ m = m
buildFluxMap' (p:processes) procToVar m =
    let
        procVar = procToVar!p
        m' = forFoldl' m (M.toList $ stoichiometry p) $ \m0 (ce, (mup, u)) ->
               let rhs = (U.RealVariableE procVar) `U.Times` (U.realConstantE u mup)
                 in
                   alterWith m0 ce $ \re0 ->
                     case re0
                       of
                         Nothing -> Just rhs
                         Just re -> Just $ re `U.Plus` rhs
    in
      buildFluxMap' processes procToVar m'

newtype ModelBuilderT m a = ModelBuilderT (S.StateT ReactionModel (U.ModelBuilderT m) a)
type ModelBuilder = ModelBuilderT I.Identity

modelBuilderTToState (ModelBuilderT a) = a

instance Monad m => Monad (ModelBuilderT m)
    where
      (ModelBuilderT a) >>= b = ModelBuilderT $ a >>= (modelBuilderTToState . b)
      return a = ModelBuilderT (return a)
      fail a = ModelBuilderT (fail a)
instance M.MonadTrans ModelBuilderT
    where
      lift a = ModelBuilderT $ M.lift $ M.lift a
instance Monad m => S.MonadState ReactionModel (ModelBuilderT m)
    where
      get = ModelBuilderT $ S.get
      put = ModelBuilderT . S.put
class ReactionModelBuilderAccess m m1 | m -> m1
    where
      liftReactions :: ModelBuilderT m1 a -> m a
instance ReactionModelBuilderAccess (ModelBuilderT m) m
    where
      liftReactions = id
instance Monad m1 => U.UnitsModelBuilderAccess (ModelBuilderT m1) m1
    where
      liftUnits = ModelBuilderT . M.lift 
instance Monad m1 => U.UnitsModelBuilderAccess (S.StateT s (ModelBuilderT m1)) m1
    where
      liftUnits = M.lift . U.liftUnits

runModelBuilderT :: Monad m => ModelBuilderT m a -> U.ModelBuilderT m (a, ReactionModel)
runModelBuilderT = flip S.runStateT emptyReactionModel . modelBuilderTToState
execModelBuilderT :: Monad m => ModelBuilderT m a -> U.ModelBuilderT m ReactionModel
execModelBuilderT = flip S.execStateT emptyReactionModel . modelBuilderTToState
runReactionBuilderInUnitBuilder :: Monad m => ModelBuilderT m a -> U.ModelBuilderT m a
runReactionBuilderInUnitBuilder mb = do
  (a, m) <- runModelBuilderT mb
  _ <- reactionModelToUnits m
  return a

runReactionBuilderInUnitBuilder' :: Monad m => ModelBuilderT m a -> U.ModelBuilderT m (M.Map CompartmentEntity U.RealVariable, M.Map Process U.RealVariable, a)
runReactionBuilderInUnitBuilder' mb = do
  (a, m) <- runModelBuilderT mb
  (ce2v, p2v) <- reactionModelToUnits m
  return (ce2v, p2v, a)

insertContextTag typetag tag v = S.modify (\m -> m {contextTaggedIDs = M.insert (typetag, tag) v (contextTaggedIDs m)})
getContextTag :: Monad m => D.TypeCode -> D.TypeCode -> ModelBuilderT m (Maybe Int)
getContextTag typetag tag = do
  idmap <- S.gets contextTaggedIDs
  return $ M.lookup (typetag, tag) idmap
contextTaggedID typetag tag wrap allocm =
    do
      t <- getContextTag typetag tag
      case t
        of
          Just id -> return $ wrap id
          Nothing ->
            do
              id <- allocateID
              allocm id
              insertContextTag typetag tag id
              return $ wrap id

allocateID :: Monad m => ModelBuilderT m Int
allocateID = S.modify (\m -> m { nextID = (+1) $ nextID m}) >> (S.gets $ flip (-) 1 . nextID)

annotateModel :: (Show a, Show b, Show c, Monad m) => a -> b -> c -> ModelBuilderT m ()
annotateModel s p o = S.modify (\m -> m { annotations = M.insert ((show s), (show p)) (show o) (annotations m) })
getAnnotation :: (Show a, Show b, Monad m) => a -> b -> ModelBuilderT m (Maybe String)
getAnnotation s p = do
  am <- S.gets annotations
  return $ M.lookup (show s, show p) am

requireNameM :: (Monad m, Show a) => ModelBuilderT m a -> String -> ModelBuilderT m a
requireNameM m n = do
    m' <- m
    annotateModel m' "nameIs" n
    return m'

getAnnotationFromModel m s p = M.lookup (show s, show p) (annotations m)

type ProcessBuilderT m a = S.StateT Process (ModelBuilderT m) a
instance Monad m1 => ReactionModelBuilderAccess (S.StateT Process (ModelBuilderT m1)) m1
    where
      liftReactions = M.lift

newExplicitProcess :: Monad m => ProcessBuilderT m a -> ModelBuilderT m Process
newExplicitProcess pb = do
  id <- allocateID
  let p0 = Process { processId = id,
                     activationCriterion = const True,
                     creatableCompartmentEntities = S.empty,
                     modifiableCompartmentEntities = S.empty,
                     entityVariables = M.empty,
                     stoichiometry = M.empty,
                     rateTemplate = U.realConstantE U.dimensionlessE 0
                   }
  p1 <- S.execStateT pb p0
  S.modify (\m -> m{explicitCompartmentProcesses = p1:(explicitCompartmentProcesses m)})
  return p1

newAllCompartmentProcess :: Monad m => (Compartment -> ProcessBuilderT m a) -> ModelBuilderT m ()
newAllCompartmentProcess f = do
  let p0 = Process { processId = 0, -- Placeholder ID...,
                     activationCriterion = const True,
                     creatableCompartmentEntities = S.empty,
                     modifiableCompartmentEntities = S.empty,
                     entityVariables = M.empty,
                     stoichiometry = M.empty,
                     rateTemplate = U.realConstantE U.dimensionlessE 0
                   }
  substCompartment <- liftM Compartment allocateID
  p1 <- S.execStateT (f substCompartment) p0
  let genFinalCompartment pid c =
          everywhere (mkT $ substituteCompartments (M.singleton substCompartment c)) (p1{processId = pid})
  S.modify (\m -> m{allCompartmentProcesses = genFinalCompartment:(allCompartmentProcesses m)})
  return ()

newAllCompartmentProcesses :: Monad m => [Compartment -> ProcessBuilderT m a] -> ModelBuilderT m ()
newAllCompartmentProcesses l = sequence_ $ map newAllCompartmentProcess l

newContainedCompartmentProcess :: Monad m => (Compartment -> Compartment -> ProcessBuilderT m a) -> ModelBuilderT m ()
newContainedCompartmentProcess f = do
  let p0 = Process { processId = 0, -- Placeholder ID...,
                     activationCriterion = const True,
                     creatableCompartmentEntities = S.empty,
                     modifiableCompartmentEntities = S.empty,
                     entityVariables = M.empty,
                     stoichiometry = M.empty,
                     rateTemplate = U.realConstantE U.dimensionlessE 0
                   }
  substCompartment1 <- liftM Compartment allocateID
  substCompartment2 <- liftM Compartment allocateID
  p1 <- S.execStateT (f substCompartment1 substCompartment2) p0
  let genFinalCompartment pid (c1, c2) =
          everywhere (mkT $ substituteCompartments $
                          M.fromList [(substCompartment1, c1),
                                      (substCompartment2, c2)]) (p1{processId=pid})
  S.modify (\m -> m{containedCompartmentProcesses = genFinalCompartment:(containedCompartmentProcesses m)})
  return ()

substituteCompartments :: M.Map Compartment Compartment -> Compartment -> Compartment
substituteCompartments m c = M.findWithDefault c c m

data IsEssentialForProcess = EssentialForProcess | NotEssentialForProcess
data CanBeCreatedByProcess = CanBeCreatedByProcess | CantBeCreatedByProcess
data CanBeModifiedByProcess = ModifiedByProcess | NotModifiedByProcess

inCompartment :: Monad m => ModelBuilderT m Entity -> ModelBuilderT m Compartment -> ModelBuilderT m CompartmentEntity
inCompartment e c = do
  e' <- e
  c' <- c
  return (e', c')

withCompartment :: Monad m => ModelBuilderT m Entity -> Compartment -> ModelBuilderT m CompartmentEntity
withCompartment e c = do
  e' <- e
  return (e', c)

addEntity :: Monad m => IsEssentialForProcess -> CanBeCreatedByProcess -> CanBeModifiedByProcess ->
                        Double -> ModelBuilderT m CompartmentEntity -> ProcessBuilderT m (U.ModelBuilderT m U.RealExpression)
addEntity essential create modify stoich mce = do
  ce@(e@(Entity u eid), c@(Compartment cid)) <- M.lift mce
  v <- U.liftUnits $ U.newRealVariable (return u)
  case essential
    of
      EssentialForProcess -> S.modify (\p->p{activationCriterion=(\s -> (S.member ce s) && (activationCriterion p s))})
      NotEssentialForProcess -> return ()
  case create
    of
      CanBeCreatedByProcess -> S.modify (\p->p{creatableCompartmentEntities=S.insert ce (creatableCompartmentEntities p)})
      CantBeCreatedByProcess -> return ()
  case modify
    of
      ModifiedByProcess -> S.modify (\p->p{modifiableCompartmentEntities=S.insert ce (modifiableCompartmentEntities p)})
      NotModifiedByProcess -> return ()
  S.modify (\p -> p{entityVariables = M.insert v ce $ entityVariables p, stoichiometry = M.insert ce (stoich, u) (stoichiometry p)})
  return $ return (U.RealVariableE v)

rateEquation :: Monad m => U.ModelBuilderT m U.RealExpression -> ProcessBuilderT m ()
rateEquation rm = do
  r <- U.liftUnits rm
  S.modify (\p->p{rateTemplate=r})

data EntityTag = EntityTag deriving (D.Typeable, D.Data)
entityTypeTag = D.typeCode EntityTag

newEntity :: Monad m => U.ModelBuilderT m U.Units -> ModelBuilderT m Entity
newEntity u = do
  id <- allocateID
  u' <- U.liftUnits u
  return $ Entity u' id

newTaggedEntity :: Monad m => U.ModelBuilderT m U.Units -> D.TypeCode -> ModelBuilderT m Entity
newTaggedEntity u tag = do
  u' <- U.liftUnits u
  contextTaggedID entityTypeTag tag (Entity u') return
newNamedEntity :: Monad m => U.ModelBuilderT m U.Units -> String -> ModelBuilderT m Entity
newNamedEntity u = requireNameM (newEntity u)
newNamedTaggedEntity :: Monad m => U.ModelBuilderT m U.Units -> D.TypeCode -> String -> ModelBuilderT m Entity
newNamedTaggedEntity u t = do
  requireNameM (newTaggedEntity u t)

data CompartmentTag = CompartmentTag deriving (D.Typeable, D.Data)
compartmentTypeTag = D.typeCode CompartmentTag

newCompartment :: Monad m => ModelBuilderT m Compartment
newCompartment = allocateID >>= return . Compartment

newTaggedCompartment :: Monad m => D.TypeCode -> ModelBuilderT m Compartment
newTaggedCompartment tag = contextTaggedID compartmentTypeTag tag Compartment return
newNamedCompartment :: Monad m => String -> ModelBuilderT m Compartment
newNamedCompartment = requireNameM newCompartment
newNamedTaggedCompartment :: Monad m => D.TypeCode -> String -> ModelBuilderT m Compartment
newNamedTaggedCompartment t = requireNameM (newTaggedCompartment t)

addContainmentM :: Monad m => Compartment -> Compartment -> ModelBuilderT m ()
addContainmentM c1 c2 = S.modify $ \m->m{containment=S.insert (c1,c2) (containment m)}
addContainmentX :: Monad m => ModelBuilderT m Compartment -> ModelBuilderT m Compartment -> ModelBuilderT m ()
addContainmentX mc1 mc2 = do
  c1 <- mc1
  c2 <- mc2
  addContainmentM c1 c2
addContainment = addContainmentX

addEntityInstance :: Monad m => ModelBuilderT m CompartmentEntity -> ModelBuilderT m EntityInstance -> ModelBuilderT m ()
addEntityInstance ce ei = do
  ce' <- ce
  ei' <- ei
  S.modify $ \m->m{entityInstances=M.insert ce' ei' (entityInstances m)}


-- Finally, also provide some Template Haskell utilities for declaring tagged Entities & Compartments...

-- | Declares a named, tagged entity for use from multiple contexts, using
-- | Template Haskell. Note that the first expression argument should have type
-- | Monad m => ModML.Units.UnitsDAEModel.ModelBuilderT m Units
-- | In addition, the qualified namespace of 
declareNamedTaggedEntity :: T.Q T.Exp -> String -> String -> T.Q [T.Dec]
declareNamedTaggedEntity expr prettyName varName = do
  expr' <- expr
  let applyUnitsName = T.AppE (T.AppE (T.AppE (T.VarE $ T.mkName "R.newNamedTaggedEntity") expr') $
                                T.VarE $ T.mkName (varName ++ "Tag"))
                         (T.LitE (T.StringL prettyName))
  U.declareTaggedSomething applyUnitsName varName

declareNamedTaggedCompartment :: String -> String -> T.Q [T.Dec]
declareNamedTaggedCompartment = U.declareNamedTaggedSomething "R.newNamedTaggedCompartment"

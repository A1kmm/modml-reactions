{-# LANGUAGE DeriveDataTypeable #-}
module ModML.Reactions.Reactions
where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Data as D
import qualified ModML.Units.UnitsDAEModel as U

{-
  A note on terminology:
  Entity represents a specific 'thing' for which some sense of quantity can
    be measured by a single real number. An entity can be an abstract
    concept like energy, or be more specific, like a particular molecule.
  Compartment describes a place at which an entity can be located, and measured.
  EntityCompartment describes a specific entity, in a specific compartment
  Amount describes how much of an entity there is in a particular compartment,
    for a particular value of the independent variable. It may be described in
    terms of concentration, or molar amounts, or charge, or charge per area or volume,
    depending on the entity and the requirements of the model.
  Process describes a transformation that can occur which depends on, and changes,
    the amount of one or more EntityCompartments. Processes describe what can
    happen, not what does happen (for example, a process may only happen in the
    presence of a particular enzyme - but the process can still be defined, even
    in a compartment where that enzyme is not present, in which case it won't
    affect the results).
  EntityInstance 
 -}

newtype Compartment = Compartment Int deriving (Eq, Ord, D.Typeable, D.Data)
newtype Entity = Entity Int deriving (Eq, Ord, D.Typeable, D.Data)
type CompartmentEntity = (Entity, Compartment)

data Process = Process {
      activationCriterion :: (S.Set CompartmentEntity) -> Bool,
      possibleProducts :: S.Set CompartmentEntity,
      entityVariables :: M.Map U.RealVariable CompartmentEntity,
      stoichiometry :: M.Map CompartmentEntity (Double, U.Units),
      rateTemplate :: U.RealExpression
    }  deriving (D.Typeable, D.Data)

data EntityInstance =
    -- The amount is always clamped to zero even if there are reactions
    -- that would produce it. Used to simplify unwanted complexity out of a model.
    EntityAbsent |
    -- The entity amount is known, but the reactions occurring don't
    -- change it. This can be used for several purposes:
    --   * If the concentration is fixed at zero, 
    EntityClamped U.RealExpression

data ReactionModel = ReactionModel {
      explicitCompartmentProcesses :: [Process],
      allCompartmentProcesses :: [Compartment -> Process],
      containedCompartmentProcesses :: [(Compartment, Compartment) -> Process],
      containment :: S.Set (Compartment, Compartment)
    }  deriving (D.Typeable, D.Data)


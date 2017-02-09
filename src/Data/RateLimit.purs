module Data.RateLimit
  ( Policy(..)

  , State
  , newState

  , checkAccess
  , recordAccess
  , attemptAccess
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Foldable (class Foldable, traverse_)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude

--------------------------------------------------------------------------------

-- | A policy describes how many accesses may occur within a particular
-- | amount of time for each key.
data Policy = Policy Milliseconds Int

derive instance eqPolicy  :: Eq  Policy
derive instance ordPolicy :: Ord Policy

--------------------------------------------------------------------------------

-- | A state keeps track of accesses.
data State k = State (Set Policy) (Ref (Map (Tuple Policy k) Int))

-- | Create a new empty state using a set of policies.
newState :: ∀ f eff k. (Foldable f) => f Policy -> Eff (ref :: REF | eff) (State k)
newState ps = State (Set.fromFoldable ps) <$> newRef Map.empty

--------------------------------------------------------------------------------

-- | Check whether access is allowed for the key.
checkAccess :: ∀ k eff. (Ord k) => State k -> k -> Eff (ref :: REF | eff) Boolean
checkAccess (State policies ref) key = do
  List.foldM checkAccess' true (List.fromFoldable policies)
  where
  checkAccess' false _ = pure false
  checkAccess' true  policy@(Policy _ max) =
    (_ <= max) <<< fromMaybe 0 <<< Map.lookup (policy /\ key) <$> readRef ref

-- | Record that access has occurred for the key. Sets timeouts to delete the
-- | access record according to the `Milliseconds` field in each policy.
recordAccess :: ∀ k eff. (Ord k) => State k -> k -> Eff (ref :: REF | eff) Unit
recordAccess (State policies ref) key = do
  modifyRef ref $ Map.mapWithKey \(_ /\ key') -> if key' == key then add 1 else id
  traverse_ upsertAccess policies
  where
  upsertAccess policy = readRef ref
                        >>= Map.lookup (policy /\ key)
                        >>> maybe (insertAccess policy) (const $ pure unit)
  insertAccess policy@(Policy expiration _) = do
    modifyRef ref $ Map.insert (policy /\ key) 1
    setTimeout expiration $
      modifyRef ref $ Map.delete (policy /\ key)

-- | `\s k -> recordAccess s k *> checkAccess s k`.
attemptAccess :: ∀ k eff. (Ord k) => State k -> k -> Eff (ref :: REF | eff) Boolean
attemptAccess s k = recordAccess s k *> checkAccess s k

foreign import setTimeout
  :: ∀ eff
   . Milliseconds
  -> Eff eff Unit
  -> Eff eff Unit

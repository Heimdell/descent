
-- | The analog of @multiplate@ package.
--
--   How to use:
--
--   Declare a bunch of mutually recursive types (see app/AST.hs),
--   add `Descent` instances.
--
--   Write some transforms, run them on with of your mutually recursive types
--   via `descending`.
--
--   Use @Control.Monad.Writer@ to simulate `fold`.
--

module Descent.Core
  ( -- * The transform
    Transform
  , Phase(..)
  , empty
  , with
  , with_
  , coercing
  , coercing_
  , add
  , pack
  , runTransform

    -- * Recursion
  , Descent (..)
  , descending
  ) where

import Control.Monad ((>=>))

import Data.Coerce (Coercible, coerce)
import Data.Typeable (Typeable, typeOf)
import Data.TypeRepMap qualified as TMap
import Data.Map qualified as Map

import Debug.Trace

data Phase
  = Entering
  | Inside
  | Leaving
  deriving stock (Eq, Ord, Show)

-- | The container for action over some node @a@.
--
newtype Action m a = MkAction
  { runAction :: a -> m a
  }

-- | The map, containing actions for various types.
--
newtype Transform m = MkTransform
  { unTransform :: Map.Map Phase (TMap.TypeRepMap (Action m))
  }

-- | Shows the list of types it has actions for.
instance Show (Transform m) where
  show = show . Map.map TMap.keys . unTransform

-- | Transform that does nothing.
--
empty :: Transform m
empty = MkTransform Map.empty

-- | Compose two transforms.
--
--   If both work on some @a@, they will be run in left-to-right order.
--
add :: (Monad m) => Transform m -> Transform m -> Transform m
add (MkTransform l) (MkTransform r) =
  MkTransform (Map.unionWith (TMap.unionWith (>->)) l r)
  where
    MkAction l' >-> MkAction r' = MkAction (l' >=> r')

-- | Chain several transforms.
--
pack :: (Monad m) => [Transform m] -> Transform m
pack = foldl add empty

-- | Convert a function to `Transform`er.
--
with :: (Typeable a, Monad m) => Phase -> (a -> m a) -> Transform m
with ph f = MkTransform (Map.singleton ph (TMap.one (MkAction f)))

with_ :: (Typeable a, Monad m) => Phase -> (a -> m ()) -> Transform m
with_ ph = with ph . sideEffect

coercing :: forall a b m. (Typeable a, Coercible a b, Monad m) => Phase -> (b -> m b) -> Transform m
coercing ph f = with ph $ fmap coerce . f . coerce @a

coercing_ :: forall a b m. (Typeable a, Coercible a b, Monad m) => Phase -> (b -> m ()) -> Transform m
coercing_ ph f = with_ ph $ f . coerce @a

-- | Run the provided side effect and leave target unchanged.
--
sideEffect :: (Typeable a, Monad m) => (a -> m ()) -> a -> m a
sideEffect f a = do
  f a
  return a

-- | Run the transform on any `Typeable`.
--
runTransform :: (Monad m, Typeable a) => Transform m -> a -> m a
runTransform t =
  runPhase Entering t >=> runPhase Inside t >=> runPhase Leaving t

runPhase :: (Monad m, Typeable a) => Phase -> Transform m -> a -> m a
runPhase phase (MkTransform trmap) a =
  case Map.lookup phase trmap >>= TMap.lookup of
    Just f  -> runAction f a
    Nothing -> pure a

-- | The interface for recurrent types.
--
class Typeable a => Descent a where
  descend
    :: forall m
    .  (Monad m)
    => (forall b. (Typeable b) => b -> m b) -- ^ the action over plain types
    -> (forall c. (Descent  c) => c -> m c) -- ^ the action over recursion points
    -> a                                    -- ^ the target of transformations
    -> m a

-- | Make the transform recure on @a@. Should be called on @a@ /immediately/.
--
--   If the `Descent` @a@ instance designates any fiend as @branch@,
--   will restart recursion on that field as well (and call enter/leave in appropriate moments).
--
descending
  :: forall a m
  .  ( Descent  a
     , Monad    m
     )
  => Transform m
  -> Transform m
descending t = add t (with Inside (go @a))
  where
    go :: forall b. (Descent b) => b -> m b
    go b = do
      -- traceShowM ("enter", typeOf b)
      r <- runPhase Entering t >=> descend visit go >=> runPhase Leaving t $ b
      -- traceShowM ("exit", typeOf b)
      return r

    visit :: forall c. (Typeable c) => c -> m c
    visit v = do
      -- traceShowM ("visit", typeOf v)
      runPhase Inside t v

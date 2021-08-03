
-- | The analog of @multiplate@ package.
--
--   How to use:
--
--   Declare a bunch of mutually recursive types (see app/AST.hs),
--   add `Descent` instances.
--
--   Write some transforms, run them on one of your mutually recursive types
--   via `descending`.
--
--   Use @Control.Monad.Writer@ to simulate `fold`.
--

module Descent.Core
  ( -- * The transform
    Transform
  , empty
  , one
  , add
  , pack
  , runTransform

    -- * Recursion
  , Descent (..)
  , descending

    -- * Helpers
  , sideEffect
  ) where

import Control.Monad ((>=>))

import Data.Typeable (Typeable)
import Data.TypeRepMap qualified as TMap

-- | The container for action over some node @a@.
--
newtype Action m a = Action
  { runAction :: a -> m a
  }

-- | The composition for action containers.
--
(>->) :: (Monad m) => Action m a -> Action m a -> Action m a
Action l >-> Action r = Action (l >=> r)

-- | The map, containing actions for various types.
--
newtype Transform m = Transform
  { unTransform :: TMap.TypeRepMap (Action m)
  }

-- | Shows the list of types it has actions for.
instance Show (Transform m) where
  show (Transform t) = show $ TMap.keys t

-- | Transform that does nothing.
--
empty :: Transform m
empty = Transform TMap.empty

-- | Compose two transforms.
--
--   If both work on some @a@, they will be run in left-to-right order.
--
add :: (Monad m) => Transform m -> Transform m -> Transform m
add (Transform l) (Transform r) =
  Transform (TMap.unionWith (>->) l r)

-- | Chain several transforms.
--
pack :: (Monad m) => [Transform m] -> Transform m
pack = foldl add empty

-- | Convert a function to `Transform`er.
--
one :: (Typeable a, Monad m) => (a -> m a) -> Transform m
one f = Transform (TMap.one (Action f))

-- | Run the provided side effect and leave target unchanged.
--
sideEffect :: (Typeable a, Monad m) => (a -> m ()) -> a -> m a
sideEffect f a = do
  f a
  return a

-- | Run the transform on any `Typeable`.
--
runTransform :: forall m. (Monad m) => Transform m -> (forall a. Typeable a => a -> m a)
runTransform (Transform trmap) a = do
  case TMap.lookup trmap of
    Just f  -> runAction f a
    Nothing -> pure a

-- | The interface for recurrent types.
--
class Typeable a => Descent a where
  descend
    :: forall m
    .  (Monad m)
    => (forall b.  Typeable b             => b -> m b) -- ^ the action over plain types
    -> (forall c. (Typeable c, Descent c) => c -> m c) -- ^ the action over recursion points
    -> a                                               -- ^ the target of transformations
    -> m a

-- | Make the transform recure on @a@. Should be called on @a@ /immediately/.
--
--   If the `Descent` @a@ instance designates any fiend as @branch@,
--   will restart recursion on that field as well (and call enter/leave in appropriate moments).
--
--   @open@ and @close@ parameters will be invoked on each implementor of @Descent@.
--
descending
  :: forall a m
  .  ( Typeable a
     , Descent  a
     , Monad    m
     )
  => Transform m  -- ^ "enter", the transform before entering recursion point
  -> Transform m  -- ^ "leave", the transform after leaving recursion point
  -> Transform m  -- ^ "payload", applied everywhere
  -> Transform m
descending open close t = add t (one (go @a))
  where
    go :: forall b. (Typeable b, Descent b, Monad m) => b -> m b
    go
      =   runTransform open
      >=> descend (runTransform t) go
      >=> runTransform close

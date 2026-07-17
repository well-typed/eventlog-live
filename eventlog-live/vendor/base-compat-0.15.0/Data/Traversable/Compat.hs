{-# LANGUAGE CPP #-}

{- |
Module      : Data.Traversable.Compat
Description : Compatibility module for `mapAccumM`.
Copyright   : (c) 2012-2018 Simon Hengel,
              (c) 2014-2018 João Cristóvão,
              (c) 2015-2018 Ryan Scott
License     : MIT
Stability   : experimental
Portability : portable
-}
module Data.Traversable.Compat (
  mapAccumM,
) where

#if MIN_VERSION_base(4,18,0)
import Data.Traversable (mapAccumM)

#else
-- !MIN_VERSION_base(4,18,0)

import Control.Monad (liftM)
import Data.Coerce (Coercible, coerce)

-- | A state transformer monad parameterized by the state and inner monad.
-- The implementation is copied from the transformers package with the
-- return tuple swapped.
--
-- /Since: 4.18.0.0/
newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

-- | /Since: 4.18.0.0/
instance Monad m => Functor (StateT s m) where
    fmap = liftM
    {-# INLINE fmap #-}

-- | /Since: 4.18.0.0/
instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (s, a)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (s', f) <- mf s
        (s'', x) <- mx s'
        return (s'', f x)
    {-# INLINE (<*>) #-}
    m *> k = m >> k
    {-# INLINE (*>) #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce

-- | /Since: 4.18.0.0/
instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        (s', a) <- (.runStateT) m s
        (.runStateT) (k a) s'
    {-# INLINE (>>=) #-}
# if !(MIN_VERSION_base(4,11,0))
    return = pure
# endif

-- | The `mapAccumM` function behaves like a combination of `mapM` and
-- `mapAccumL` that traverses the structure while evaluating the actions
-- and passing an accumulating parameter from left to right.
-- It returns a final value of this accumulator together with the new structure.
-- The accummulator is often used for caching the intermediate results of a computation.
--
--  @since 4.18.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let expensiveDouble a = putStrLn ("Doubling " <> show a) >> pure (2 * a)
-- >>> :{
-- mapAccumM (\cache a -> case lookup a cache of
--     Nothing -> expensiveDouble a >>= \double -> pure ((a, double):cache, double)
--     Just double -> pure (cache, double)
--     ) [] [1, 2, 3, 1, 2, 3]
-- :}
-- Doubling 1
-- Doubling 2
-- Doubling 3
-- ([(3,6),(2,4),(1,2)],[2,4,6,2,4,6])
--
mapAccumM
  :: forall m t s a b. (Monad m, Traversable t)
  => (s -> a -> m (s, b))
  -> s -> t a -> m (s, t b)
mapAccumM f s t = (.runStateT) (mapM (StateT #. flip f) t) s

-- !MIN_VERSION_base(4,18,0)
#endif

-- |
--
-- Module:      Control.Egison.Matcher
-- Description: Matcher class and basic matchers
-- Stability:   experimental
--
-- This module defines a class for matchers and some basic matchers.

module Control.Egison.Matcher
  ( Matcher
  , Something(..)
  , ValuePattern(..)
  , Eql(..)
  , CollectionPattern(..)
  , List(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )

-- | Class for matchers. @'Matcher' m tgt@ denotes that @m@ is a matcher for @tgt@.
class Matcher m tgt

-- | Matcher that handles pattern variables and wildcards for arbitrary types.
data Something = Something

instance Matcher Something a


-- | Type synonym for patterns.
type Pattern im it om ot = im -> it -> [(om, ot)]

class Eq t => ValuePattern m t where
  value :: t -> Pattern m t () ()

-- | Matcher that can handle value patterns of 'Eq' types.
data Eql = Eql

instance Eq a => Matcher Eql a
instance Eq a => ValuePattern Eql a where
  value e _ v = if e == v then pure ((), ()) else mzero

-- | Class for collection pattern constructors.
class CollectionPattern m t where
  -- | Matcher for the elements.
  type ElemM m
  -- | Type of the target elements.
  type ElemT t
  -- | Pattern that matches with empty collections.
  -- @[]@ is desugared into 'nil' by the quasi-quoter.
  nil :: Pattern m t () ()
  -- | Pattern that destructs collections into its head and tail.
  -- @:@ is desugared into 'cons' by the quasi-quoter.
  cons :: Pattern m t (ElemM m, m) (ElemT t, t)
  -- | Pattern that destructs collections into its initial prefix and remaining suffix.
  -- @++@ is desugared into 'join' by the quasi-quoter.
  join :: Pattern m t (m, m) (t, t)

-- | 'List' matcher is a matcher for collections that matches as if they're normal lists.
newtype List m = List m

instance Matcher m tgt => Matcher (List m) [tgt]

instance Matcher m t => CollectionPattern (List m) [t] where
  type ElemM (List m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ [] = pure ((), ())
  nil _ _ = mzero
  {-# INLINE cons #-}
  cons _ [] = mzero
  cons (List m) (x : xs) = pure ((m, List m), (x, xs))
  {-# INLINABLE join #-}
  join m []       = pure ((m, m), ([], []))
  join m (x : xs) = pure ((m, m), ([], x : xs)) `mplus` do
    (om, (ys, zs)) <- join m xs
    pure (om, (x : ys, zs))

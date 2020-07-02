-- |
--
-- Module:      Control.Egison.Matcher.Collection
-- Description: Matchers for collection data types
-- Stability:   experimental

module Control.Egison.Matcher.Collection
  ( CollectionPattern(..)
  , List(..)
  , Multiset(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search
import           Control.Egison.Match
import           Control.Egison.Matcher
import           Control.Egison.QQ

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

instance Matcher m t => Matcher (List m) [t]

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

newtype Multiset m = Multiset m

instance Matcher m t => Matcher (Multiset m) [t]

instance Matcher m t => CollectionPattern (Multiset m) [t] where
  type ElemM (Multiset m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ [] = pure ((), ())
  nil _ _ = mzero
  {-# INLINE cons #-}
  cons (Multiset m) xs = map (\(y, ys) -> ((m, Multiset m), (y, ys))) $ matchAll dfs xs (List Something) [[mc| $hs ++ $x : $ts -> (x, hs ++ ts) |]]
  {-# INLINABLE join #-}
  join = undefined

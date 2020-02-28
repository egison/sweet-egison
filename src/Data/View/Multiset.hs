module Data.View.Multiset
  ( Multiset(..)
  )
where

import           Data.View                      ( View(..) )
import           Control.Monad                  ( MonadPlus(..) )

import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )


newtype Multiset a = Multiset [a]

instance View a => View (Multiset a) where
  type Target (Multiset a) = [Target a]
  type Projection (Multiset a) = Multiset (Projection a)

instance CollectionPattern (Multiset a) where
  type Element (Multiset a) = a
  cons (Multiset xs) = go xs [] mzero
   where
    go [] _ acc = acc
    go (x : xs') rest acc =
      pure (x, Multiset (rest ++ xs')) `mplus` go xs' (rest ++ [x]) acc
-- TODO: Implement
  join = undefined

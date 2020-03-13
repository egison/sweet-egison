module Control.Egison.Matcher.Multiset
  ( Multiset(..)
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad                  ( MonadPlus(..) )

import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )


newtype Multiset a = Multiset [a]

instance Matcher a => Matcher (Multiset a) where
  type Target (Multiset a) = [Target a]
  {-# INLINE wrap #-}
  wrap = Multiset . map wrap
  {-# INLINE unwrap #-}
  unwrap (Multiset xs) = map unwrap xs

instance CollectionPattern (Multiset a) where
  type Element (Multiset a) = a
  {-# INLINABLE nil #-}
  nil (Multiset []) = pure ()
  nil _             = mzero
  {-# INLINABLE cons #-}
  cons (Multiset xs) = go xs [] mzero
   where
    go [] _ acc = acc
    go (x : xs') rest acc =
      pure (x, Multiset (rest ++ xs')) `mplus` go xs' (rest ++ [x]) acc
-- TODO: Implement
  join   = undefined
  spread = undefined

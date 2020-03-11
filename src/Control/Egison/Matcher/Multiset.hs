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
  wrap = Multiset . map wrap
  unwrap (Multiset xs) = map unwrap xs

instance CollectionPattern (Multiset a) where
  type Element (Multiset a) = a
  nil (Multiset []) = pure ()
  nil _             = mzero
  cons (Multiset xs) = go xs [] mzero
   where
    go [] _ acc = acc
    go (x : xs') rest acc =
      pure (x, Multiset (rest ++ xs')) `mplus` go xs' (rest ++ [x]) acc
-- TODO: Implement
  join   = undefined
  spread = undefined

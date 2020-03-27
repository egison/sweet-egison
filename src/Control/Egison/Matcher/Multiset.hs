module Control.Egison.Matcher.Multiset
  ( Multiset(..)
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )
import           Data.Tagged                    ( Tagged(..) )


newtype Multiset m = Multiset m

instance Matcher m tgt => Matcher (Multiset m) [tgt]

instance Matcher m tgt => CollectionPattern (Multiset m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (Multiset m) = m
  {-# INLINABLE nil #-}
  nil (Tagged []) = pure ()
  nil _           = mzero
  {-# INLINABLE cons #-}
  cons (Tagged xs) = go xs [] mzero
   where
    go [] _ acc = acc
    go (x : xs') rest acc =
      pure (Tagged x, Tagged (rest ++ xs')) `mplus` go xs' (rest ++ [x]) acc
-- TODO: Implement
  join   = undefined
  spread = undefined

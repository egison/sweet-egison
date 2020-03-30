module Control.Egison.Matcher.Multiset
  ( Multiset(..)
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )


newtype Multiset m = Multiset m

instance Matcher m tgt => Matcher (Multiset m) [tgt]

instance Matcher m tgt => CollectionPattern (Multiset m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (Multiset m) = m
  {-# INLINABLE nil #-}
  nil _ [] = pure ()
  nil _ _  = mzero
  {-# INLINABLE cons #-}
  cons _ xs = go xs [] mzero
   where
    go [] _ acc = acc
    go (x : xs') rest acc =
      pure (x, rest ++ xs') `mplus` go xs' (rest ++ [x]) acc
-- TODO: Implement
  join   = undefined
  spread = undefined

module Control.Egison.Matcher.List
  ( List(..)
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )


newtype List m = List m

instance Matcher m tgt => Matcher (List m) [tgt]

instance Matcher m tgt => CollectionPattern (List m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (List m) = m
  {-# INLINE nil #-}
  nil _ [] = pure ()
  nil _ _  = mzero
  {-# INLINE cons #-}
  cons _ []       = mzero
  cons _ (x : xs) = pure (x, xs)
  {-# INLINABLE join #-}
  join _ []       = pure ([], [])
  join t (x : xs) = pure ([], x : xs) `mplus` do
    (ys, zs) <- join t xs
    pure (x : ys, zs)
  {-# INLINABLE spread #-}
  spread _ []       = mzero
  spread t (x : xs) = pure (x : xs) `mplus` spread t xs

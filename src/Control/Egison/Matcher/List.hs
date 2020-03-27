module Control.Egison.Matcher.List
  ( List(..)
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )
import           Data.Tagged                    ( Tagged(..) )


newtype List m = List m

instance Matcher m tgt => Matcher (List m) [tgt]

instance Matcher m tgt => CollectionPattern (List m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (List m) = m
  {-# INLINE nil #-}
  nil (Tagged []) = pure ()
  nil _           = mzero
  {-# INLINE cons #-}
  cons (Tagged []      ) = mzero
  cons (Tagged (x : xs)) = pure (Tagged x, Tagged xs)
  {-# INLINABLE join #-}
  join (Tagged []      ) = pure (Tagged [], Tagged [])
  join (Tagged (x : xs)) = pure (Tagged [], Tagged (x : xs)) `mplus` do
    (Tagged ys, zs) <- join (Tagged xs)
    pure (Tagged (x : ys), zs)
  {-# INLINABLE spread #-}
  spread (Tagged []      ) = mzero
  spread (Tagged (x : xs)) = pure (Tagged (x : xs)) `mplus` spread (Tagged xs)

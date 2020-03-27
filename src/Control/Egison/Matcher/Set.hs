module Control.Egison.Matcher.Set
  ( Set(..)
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )
import           Data.Tagged                    ( Tagged(..) )


newtype Set m = Set m

instance Matcher m tgt => Matcher (Set m) [tgt]

instance Matcher m tgt => CollectionPattern (Set m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (Set m) = m
  {-# INLINABLE nil #-}
  nil (Tagged []) = pure ()
  nil _           = mzero
  {-# INLINABLE cons #-}
  cons (Tagged xs) = foldr go mzero xs
    where go x acc = pure (Tagged x, Tagged xs) `mplus` acc
-- TODO: Implement
  join   = undefined
  spread = undefined

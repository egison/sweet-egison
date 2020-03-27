module Control.Egison.Matcher.Set
  ( Set(..)
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad                  ( MonadPlus(..) )
import           Data.Query.Pattern             ( ReturnList(..) )
import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )


newtype Set m = Set m

instance Matcher m tgt => Matcher (Set m) [tgt]

instance Matcher m tgt => CollectionPattern (Set m) [tgt] where
  type Elem [tgt] = tgt
  type ElemTag (Set m) = m
  {-# INLINABLE nil #-}
  nil _ [] = pure Nil
  nil _ _  = mzero
  {-# INLINABLE cons #-}
  cons _ xs = foldr go mzero xs
    where go x acc = pure (x :- xs :- Nil) `mplus` acc
-- TODO: Implement
  join   = undefined
  spread = undefined

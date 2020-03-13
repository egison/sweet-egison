module Control.Egison.Matcher.Set
  ( Set(..)
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad                  ( MonadPlus(..) )

import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )


newtype Set a = Set [a]

instance Matcher a => Matcher (Set a) where
  type Target (Set a) = [Target a]
  {-# INLINE wrap #-}
  wrap x = Set $ map wrap x
  {-# INLINE unwrap #-}
  unwrap (Set x) = map unwrap x

instance CollectionPattern (Set a) where
  type Element (Set a) = a
  {-# INLINABLE nil #-}
  nil (Set []) = pure ()
  nil _        = mzero
  {-# INLINABLE cons #-}
  cons (Set xs) = foldr go mzero xs
    where go x acc = pure (x, Set xs) `mplus` acc
-- TODO: Implement
  join   = undefined
  spread = undefined

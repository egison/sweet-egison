module Control.Egison.Matcher.List
  ( List(..)
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad                  ( MonadPlus(..) )

import           Data.Query.Pattern.Collection  ( CollectionPattern(..) )


newtype List a = List [a]

instance Matcher a => Matcher (List a) where
  type Target (List a) = [Target a]
  {-# INLINE wrap #-}
  wrap = List . map wrap
  {-# INLINE unwrap #-}
  unwrap (List xs) = map unwrap xs

instance CollectionPattern (List a) where
  type Element (List a) = a
  {-# INLINE nil #-}
  nil (List []) = pure ()
  nil _         = mzero
  {-# INLINE cons #-}
  cons (List []      ) = mzero
  cons (List (x : xs)) = pure (x, List xs)
  {-# INLINABLE join #-}
  join (List []      ) = pure (List [], List [])
  join (List (x : xs)) = pure (List [], List (x : xs)) `mplus` do
    (List ys, zs) <- join (List xs)
    pure (List (x : ys), zs)
  {-# INLINABLE spread #-}
  spread (List []      ) = mzero
  spread (List (x : xs)) = pure (List (x : xs)) `mplus` spread (List xs)

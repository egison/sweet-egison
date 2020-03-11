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
  wrap = List . map wrap
  unwrap (List xs) = map unwrap xs

instance CollectionPattern (List a) where
  type Element (List a) = a
  nil (List []) = pure ()
  nil _         = mzero
  cons (List []      ) = mzero
  cons (List (x : xs)) = pure (x, List xs)
  join (List []      ) = pure (List [], List [])
  join (List (x : xs)) = pure (List [], List (x : xs)) `mplus` do
    (List ys, zs) <- join (List xs)
    pure (List (x : ys), zs)
  spread (List []      ) = mzero
  spread (List (x : xs)) = pure (List (x : xs)) `mplus` spread (List xs)

module Data.Query.Pattern.Collection
  ( CollectionPattern(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch )


class CollectionPattern a where
  type Element a
  cons :: MonadSearch m => a -> m (Element a, a)
  join :: MonadSearch m => a -> m (a, a)

instance CollectionPattern [a] where
  type Element [a] = a
  cons []       = mzero
  cons (x : xs) = pure (x, xs)
  join []       = pure ([], [])
  join (x : xs) = pure ([], x : xs) `mplus` do
    (ys, zs) <- join xs
    pure (x : ys, zs)

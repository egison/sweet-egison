module Data.Query.Pattern.Collection
  ( CollectionPattern(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch )


class CollectionPattern a where
  type Element a
  nil :: MonadSearch m => a -> m ()
  cons :: MonadSearch m => a -> m (Element a, a)
  join :: MonadSearch m => a -> m (a, a)
  spread :: MonadSearch m => a -> m a

instance CollectionPattern [a] where
  type Element [a] = a
  nil [] = pure ()
  nil _  = mzero
  cons []       = mzero
  cons (x : xs) = pure (x, xs)
  join []       = pure ([], [])
  join (x : xs) = pure ([], x : xs) `mplus` do
    (ys, zs) <- join xs
    pure (x : ys, zs)
  spread []       = mzero
  spread (x : xs) = pure (x : xs) `mplus` spread xs

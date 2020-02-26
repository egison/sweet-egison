module Control.Egison.Pattern
  ( CollectionPattern(..)
  )
where

import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch )


class CollectionPattern a where
  type Element a
  cons :: MonadSearch m => a -> m (Element a, a)
  tails :: MonadSearch m => a -> m a

instance CollectionPattern [a] where
  type Element [a] = a
  cons []       = mzero
  cons (x : xs) = pure (x, xs)
  tails []       = mzero
  tails (x : xs) = pure (x : xs) `mplus` tails xs

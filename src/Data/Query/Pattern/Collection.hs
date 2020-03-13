module Data.Query.Pattern.Collection
  ( CollectionPattern(..)
  )
where

import           Control.Monad.Search           ( MonadSearch )


class CollectionPattern a where
  type Element a
  nil :: MonadSearch m => a -> m ()
  cons :: MonadSearch m => a -> m (Element a, a)
  join :: MonadSearch m => a -> m (a, a)
  spread :: MonadSearch m => a -> m a

module Data.Query.Pattern.Collection
  ( CollectionPattern(..)
  )
where

import           Control.Monad.Search           ( MonadSearch )
import           Data.Tagged                    ( Tagged )


class CollectionPattern t a where
  type Elem a
  type ElemTag t
  nil :: MonadSearch m => Tagged t a -> m ()
  cons :: MonadSearch m => Tagged t a -> m (Tagged (ElemTag t) (Elem a), Tagged t a)
  join :: MonadSearch m => Tagged t a -> m (Tagged t a, Tagged t a)
  spread :: MonadSearch m => Tagged t a -> m (Tagged t a)

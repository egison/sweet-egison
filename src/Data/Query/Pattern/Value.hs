module Data.Query.Pattern.Value
  ( ValuePattern(..)
  )
where

import           Control.Monad.Search           ( MonadSearch )
import           Data.Tagged                    ( Tagged )


class ValuePattern t a where
  value :: MonadSearch m => Tagged t a -> a -> m ()

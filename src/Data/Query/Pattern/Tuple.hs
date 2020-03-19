module Data.Query.Pattern.Tuple
  ( Tuple2Pattern(..)
  )
where

import           Control.Monad.Search           ( MonadSearch )


class Tuple2Pattern a where
  type Fst a
  type Snd a
  tuple2 :: MonadSearch m => a -> m (Fst a, Snd a)

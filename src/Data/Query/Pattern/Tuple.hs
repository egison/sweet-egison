module Data.Query.Pattern.Tuple
  ( Tuple2Pattern(..)
  )
where

import           Control.Monad.Search           ( MonadSearch )
import           Data.Tagged                    ( Tagged )


class Tuple2Pattern t a where
  type Fst a
  type Snd a
  type FstTag t
  type SndTag t
  tuple2 :: MonadSearch m => Tagged t a -> m (Tagged (FstTag t) (Fst a), Tagged (SndTag t) (Snd a))

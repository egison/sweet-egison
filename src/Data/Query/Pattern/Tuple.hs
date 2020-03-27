module Data.Query.Pattern.Tuple
  ( Tuple2Pattern(..)
  )
where

import           Data.Query.Pattern             ( Pattern )


class Tuple2Pattern t a where
  type Fst a
  type Snd a
  type FstTag t
  type SndTag t
  tuple2 :: Pattern t a '[FstTag t, SndTag t] '[Fst a, Snd a]

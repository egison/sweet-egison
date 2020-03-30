module Data.Query.Pattern.Value
  ( ValuePattern(..)
  )
where

import           Control.Monad                  ( mzero )
import           Data.Query.Pattern             ( Pattern )


class ValuePattern t a where
  value :: a -> Pattern t a '[] '[]

  default value :: Eq a => a -> Pattern t a '[] '[]
  value a _ b | a == b    = pure ()
              | otherwise = mzero

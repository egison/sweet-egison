module Data.Query.Pattern.Value
  ( ValuePattern(..)
  )
where

import           Data.Query.Pattern             ( Pattern )


class ValuePattern t a where
  value :: a -> Pattern t a '[] '[]

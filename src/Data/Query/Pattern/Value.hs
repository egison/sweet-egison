module Data.Query.Pattern.Value
  ( ValuePattern(..)
  )
where

import           Data.Tagged                    ( Tagged )


class ValuePattern t a where
  value :: Tagged t a -> a -> Bool

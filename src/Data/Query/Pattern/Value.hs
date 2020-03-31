-- |
--
-- Module:      Data.Query.Pattern.Value
-- Description: Value patterns
-- Stability:   experimental
--
-- Overloaded name definitions for value patterns.

module Data.Query.Pattern.Value
  ( ValuePattern(..)
  )
where

import           Control.Monad                  ( mzero )
import           Data.Query.Pattern             ( Pattern )


-- | Class for value patterns.
class ValuePattern t a where
  -- | Takes an expression to compare, and returns a pattern that matches to the expression.
  value :: a -> Pattern t a '[] '[]

  default value :: Eq a => a -> Pattern t a '[] '[]
  -- | Default definition uses '==' to compare values.
  value a _ b | a == b    = pure ()
              | otherwise = mzero

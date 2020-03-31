-- |
--
-- Module:      Data.Query.Pattern.Collection
-- Description: Patterns for collections
-- Stability:   experimental
--
-- Overloaded pattern constructors for collection types.

module Data.Query.Pattern.Collection
  ( CollectionPattern(..)
  )
where

import           Data.Query.Pattern             ( Pattern )


-- | Class for collection pattern constructors.
class CollectionPattern t a where
  -- | Element type of @a@.
  type Elem a
  -- | Element tag type of @t@.
  type ElemTag t
  -- | Pattern that matches with empty collections.
  -- @[]@ is desugared into 'nil' by the quasi-quoter.
  nil :: Pattern t a '[] '[]
  -- | Pattern that destructs collections into its head and tail.
  -- @:@ is desugared into 'cons' by the quasi-quoter.
  cons :: Pattern t a '[ElemTag t, t] '[Elem a, a]
  -- | Pattern that destructs collections into its initial prefix and remaining suffix.
  -- @++@ is desugared into 'join' by the quasi-quoter.
  join :: Pattern t a '[t, t] '[a, a]
  -- | Pattern that destructs collections into its suffixes.
  -- Some patterns with 'join' are optimized using 'spread'.
  spread :: Pattern t a '[t] '[a]

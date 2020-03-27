module Data.Query.Pattern.Collection
  ( CollectionPattern(..)
  )
where

import           Data.Query.Pattern             ( Pattern )


class CollectionPattern t a where
  type Elem a
  type ElemTag t
  nil :: Pattern t a '[] '[]
  cons :: Pattern t a '[ElemTag t, t] '[Elem a, a]
  join :: Pattern t a '[t, t] '[a, a]
  spread :: Pattern t a '[t] '[a]

module Data.View
  ( View(..)
  , Plain(..)
  , as
  )
where

import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )


class Coercible (Target a) (Projection a) => View a where
  type Target a
  type Projection a

as :: (View v, a ~ Target v) => a -> (x -> v) -> Projection v
as x _ = coerce x

infixl 2 `as`


newtype Plain a = Plain a

instance View (Plain a) where
  type Target (Plain a) = a
  type Projection (Plain a) = a

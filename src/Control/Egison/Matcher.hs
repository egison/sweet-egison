module Control.Egison.Matcher
  ( Matcher(..)
  , M(..)
  )
where

import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )


class Matcher a where
  type Target a
  wrap :: Target a -> a
  unwrap :: a -> Target a

  default wrap :: Coercible (Target a) a => Target a -> a
  wrap = coerce
  default unwrap :: Coercible a (Target a) => a -> Target a
  unwrap = coerce


newtype M a = M a
  deriving newtype Eq

instance Matcher (M a) where
  type Target (M a) = a

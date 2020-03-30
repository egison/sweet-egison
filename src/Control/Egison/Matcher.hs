module Control.Egison.Matcher
  ( Matcher
  , Something(..)
  , EqM(..)
  , IntegralM(..)
  )
where

import           Data.Query.Pattern.Value       ( ValuePattern(..) )


class Matcher m tgt


data Something = Something

instance Matcher Something a


data EqM = EqM

instance Eq a => Matcher EqM a
instance Eq a => ValuePattern EqM a


data IntegralM = IntegralM

instance Integral a => Matcher IntegralM a
instance Integral a => ValuePattern IntegralM a

-- |
--
-- Module:      Control.Egison.Matcher
-- Description: Matcher class and basic matchers
-- Stability:   experimental
--
-- This module defines a class for matchers and some basic matchers.

module Control.Egison.Matcher
  ( Matcher
  , Something(..)
  , EqM(..)
  , IntegralM(..)
  )
where

import           Data.Query.Pattern.Value       ( ValuePattern(..) )


-- | Class for matchers. @'Matcher' m tgt@ denotes that @m@ is a matcher for @tgt@.
class Matcher m tgt


-- | Matcher that handles pattern variables and wildcards for arbitrary types.
data Something = Something

instance Matcher Something a


-- | Matcher that can handle value patterns of 'Eq' types.
data EqM = EqM

instance Eq a => Matcher EqM a
instance Eq a => ValuePattern EqM a


-- | Matcher for 'Integral' types.
data IntegralM = IntegralM

instance Integral a => Matcher IntegralM a
instance Integral a => ValuePattern IntegralM a

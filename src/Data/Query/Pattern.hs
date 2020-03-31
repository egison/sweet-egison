-- |
--
-- Module:      Data.Query.Pattern
-- Description: Definitions that support pattern definitions and applications
-- Stability:   experimental
--
-- This module defines names that support concise pattern definitions and applications.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Data.Query.Pattern
  ( Pattern
  , apply
  )
where

import           Control.Monad.Search           ( MonadSearch )
import           Data.Kind                      ( Type )
import           Data.Proxy                     ( Proxy(..) )

import           Data.Query.Pattern.InternalTH  ( makeToTupleInstance )


data HList (xs :: [Type]) where
  HNil ::HList '[]
  HCons ::x -> HList xs -> HList (x ': xs)

class ToTuple (xs :: [Type]) where
  type Tupled xs :: Type
  tupled :: HList xs -> Tupled xs

$(traverse makeToTupleInstance [0..7])

class ToProxyList (x :: [Type]) where
  type ProxyList x :: [Type]
  proxyList :: HList (ProxyList x)

instance ToProxyList '[] where
  type ProxyList '[] = '[]
  proxyList = HNil

instance ToProxyList xs => ToProxyList (x ': xs) where
  type ProxyList (x ': xs) = Proxy x ': ProxyList xs
  proxyList = HCons Proxy (proxyList @xs)

-- | Apply patterns. This combinator is internally used in the spliced expression of quasi-quoters.
--
-- We can use 'apply' to call patterns explicitly: @'apply' pat tag tgt >-> \(tagOut, tgtOut) -> e@
-- where @tagOut@ is a tuple pattern that binds output tags and @tgtOut@ is also a tuple pattern that binds output values.
--
-- 'apply' plays an important role that moves output tag types from the patterns' parameter to the returning value.
apply
  :: forall tag tgt tagOut tgtOutTuple m
   . (MonadSearch m, ToTuple (ProxyList tagOut), ToProxyList tagOut)
  => ((Proxy tag, Proxy tagOut) -> tgt -> m tgtOutTuple)
  -> (Proxy tag -> tgt -> m (Tupled (ProxyList tagOut), tgtOutTuple))
apply pat tag tgt = (proxyTup, ) <$> pat (tag, Proxy) tgt
  where proxyTup = tupled (proxyList @tagOut)

-- | Type synonym for patterns.
--
-- @'Pattern' tag tgt tagOut tgtOut@ is a type of a pattern that takes values of type @tgt@ tagged with @tag@, and returns @tgtOut@ tagged with @tagOut@.
-- @tagOut@ is embedded in the parameter of 'Pattern' to make pattern definitions direct and concise.
type Pattern tag tgt tagOut tgtOut
  =  forall m
   . MonadSearch m
  => (Proxy tag, Proxy tagOut)
  -> tgt
  -> m (Tupled tgtOut)

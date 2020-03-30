{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Data.Query.Pattern
  ( Pattern
  , apply
  )
where

import           Control.Monad.Search           ( MonadSearch )
import           Data.Kind                      ( Type )
import           Data.Proxy                     ( Proxy(..) )


data HList (xs :: [Type]) where
  HNil ::HList '[]
  HCons ::x -> HList xs -> HList (x ': xs)

class ToTuple (xs :: [Type]) where
  type Tupled xs :: Type
  tupled :: HList xs -> Tupled xs

instance ToTuple '[] where
  type Tupled '[] = ()
  tupled HNil = ()

instance ToTuple '[a] where
  type Tupled '[a] = a
  tupled (HCons x1 HNil) = x1

instance ToTuple '[t1, t2] where
  type Tupled '[t1, t2] = (t1, t2)
  tupled (HCons x1 (HCons x2 HNil)) = (x1, x2)

instance ToTuple '[t1, t2, t3] where
  type Tupled '[t1, t2, t3] = (t1, t2, t3)
  tupled (HCons x1 (HCons x2 (HCons x3 HNil))) = (x1, x2, x3)

class ToProxyList (x :: [Type]) where
  type ProxyList x :: [Type]
  proxyList :: HList (ProxyList x)

instance ToProxyList '[] where
  type ProxyList '[] = '[]
  proxyList = HNil

instance ToProxyList xs => ToProxyList (x ': xs) where
  type ProxyList (x ': xs) = Proxy x ': ProxyList xs
  proxyList = HCons Proxy (proxyList @xs)

apply
  :: forall tag tgt tagOut tgtOutTuple m
   . (MonadSearch m, ToTuple (ProxyList tagOut), ToProxyList tagOut)
  => ((Proxy tag, Proxy tagOut) -> tgt -> m tgtOutTuple)
  -> (Proxy tag -> tgt -> m (Tupled (ProxyList tagOut), tgtOutTuple))
apply pat tag tgt = (proxyTup, ) <$> pat (tag, Proxy) tgt
  where proxyTup = tupled (proxyList @tagOut)

type Pattern tag tgt tagOut tgtOut
  =  forall m
   . MonadSearch m
  => (Proxy tag, Proxy tagOut)
  -> tgt
  -> m (Tupled tgtOut)

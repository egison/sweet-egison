{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

import           Control.Egison
import           System.Environment
import           Control.Monad.Search
import           Data.List               hiding ( delete )
import           Data.Maybe

delete x xs =
  match dfs xs (List Eql) [[mc| $hs ++ #x : $ts -> hs ++ ts |], [mc| _ -> xs |]]

delete' x xs =
  match dfs xs (Multiset Eql) [[mc| $x : $ts -> ts |], [mc| _ -> xs |]]

deleteNative _ []                = []
deleteNative x (y : ys) | x == y = ys
deleteNative x (y : ys)          = y : deleteNative x ys

main = do
  [fn, m, n] <- getArgs
  let fn' = read fn :: Int
  let m'  = read m :: Int
  let n'  = read n :: Int
  case fn' of
    -- m'=9000, n'=10000: 0.025
    1 -> print $ length $ delete m' [1 .. n']
    -- m'=9000, n'=10000: 0.025
    2 -> print $ length $ delete' m' [1 .. n']
    -- m'=9000, n'=10000: 0.025
    3 -> print $ length $ deleteNative m' [1 .. n']

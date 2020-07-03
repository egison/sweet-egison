{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

import           Control.Egison
import           System.Environment
import           Control.Monad.Search
import           Data.List
import           Data.Maybe

perm2 :: Int -> [(Int, Int)]
perm2 n =
  matchAll dfs [1 .. n] (Multiset Something) [[mc| $x : $y : _ -> (x, y) |]]

perm2Native :: Int -> [(Int, Int)]
perm2Native n = go [1 .. n] [] []
 where
  go [] _ acc = acc
  go (x : xs) rest acc =
    [ (x, y) | y <- rest ++ xs ] ++ go xs (rest ++ [x]) acc

main = do
  [fn, n] <- getArgs
  let fn' = read fn :: Int
  let n' = read n :: Int
  case fn' of
    -- n'=5000, 0.811s
    1 -> print $ length $ perm2 n'
    -- n'=5000, 0.404s
    2 -> print $ length $ perm2Native n'
--    3 -> print $ length $ perm2Backtracking n'

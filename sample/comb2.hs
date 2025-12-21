{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Egison
import Data.List
import Data.Maybe
import System.Environment

comb2 :: Int -> [(Int, Int)]
comb2 n =
  matchAll
    dfs
    [1 .. n]
    (List Something)
    [[mc| _ ++ $x : _ ++ $y : _ -> (x, y) |]]

comb2Native :: Int -> [(Int, Int)]
comb2Native n = [(x, y) | (x : ts) <- tails [1 .. n], y <- ts]

comb2Backtracking :: Int -> [(Int, Int)]
comb2Backtracking n =
  toList $
    dfs [1 .. n]
      >>= fromList
        . tails
      >>= (fromList . maybeToList . uncons)
      >>= \(x, ys) ->
        fromList (tails ys)
          >>= (\ys' -> fromList (maybeToList (uncons ys)))
          >>= \(y, _) -> pure (x, y)

main = do
  [fn, n] <- getArgs
  let fn' = read fn :: Int
  let n' = read n :: Int
  case fn' of
    -- n'=15000, 0.303
    1 -> print $ length $ comb2 n'
    -- n'=15000, 0.347
    2 -> print $ length $ comb2Native n'
    -- n'=15000, 0.364
    3 -> print $ length $ comb2Backtracking n'

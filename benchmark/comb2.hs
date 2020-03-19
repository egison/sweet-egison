import           Control.Egison
import           Control.Egison.Matcher.List

import           Data.List                      ( tails )
import           Criterion.Main


comb2 :: Int -> [(Int, Int)]
comb2 n =
  matchAll @DFS [1 .. n] @(List (M Int)) [q| _ ++ $x : _ ++ $y : _ -> (x, y) |]

comb2Native :: Int -> [(Int, Int)]
comb2Native n = [ (y, z) | y : ts <- tails xs, z <- ts ] where xs = [1 .. n]

main :: IO ()
main = defaultMain
  [ bgroup
      "comb2"
      [ bgroup
        "1600"
        [ bench "native" $ nf comb2Native 1600
        , bench "sweet-egison" $ nf comb2 1600
        ]
      , bgroup
        "3200"
        [ bench "native" $ nf comb2Native 3200
        , bench "sweet-egison" $ nf comb2 3200
        ]
      , bgroup
        "6400"
        [ bench "native" $ nf comb2Native 6400
        , bench "sweet-egison" $ nf comb2 6400
        ]
      ]
  ]

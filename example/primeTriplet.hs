{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           Control.Egison.Matcher.List
import           Data.Numbers.Primes            ( primes )


main :: IO ()
main = print $ take 10 results
 where
  results = matchAll @BFS @(List (M Int)) primes [q|
      _ ++ $x : ($y & (#(x + 2) | #(x + 4))) : #(x + 6) : _ -> (x, y, x + 6)
    |]

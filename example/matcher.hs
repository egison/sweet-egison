{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           Control.Egison.Matcher.Set


main :: IO ()
main = print $ take 10 results
 where
  results = matchAll @BFS @(Set (M Int))
    [1 ..]
    [q|
      $x : $y : _ -> (x, y)
    |]

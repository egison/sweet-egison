{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           Data.View.Set


main :: IO ()
main = print $ take 10 results
 where
  results = matchAll @BFS $ [1 ..] `as` Set @(Plain Int) `with` [query|
      $x : $y : _ => (x, y)
    |]

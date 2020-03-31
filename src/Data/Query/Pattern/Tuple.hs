-- |
--
-- Module:      Data.Query.Pattern.Tuple
-- Description: Patterns for tuples
-- Stability:   experimental
--
-- Overloaded pattern constructors for tuple types.
-- @(p1, p2, .., pn)@ is desugared into @tuplen p1 p2 .. pn@ by the quasi-quoter. e.g. @(p1, p2)@ → @'tuple2' p1 p2@

{-# LANGUAGE TemplateHaskell #-}

module Data.Query.Pattern.Tuple where

import           Data.Query.Pattern             ( Pattern )

import           Data.Query.Pattern.InternalTH  ( makeTuplePatternClass )


$(traverse makeTuplePatternClass $ 0 : [2..7])

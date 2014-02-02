{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main (Properties)
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Main where

import Data.Text (pack)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

import Data.Spellcheck.Distance

prop_dist_equals_0_on_same_input x =
    let x' = pack x in editDistance False x' x' == 0

prop_dist_trans_is_lower_or_equal x y =
    let x' = pack x
        y' = pack y in
    editDistance True x' y' <= editDistance False x' y'

prop_dist_is_symetric x y b =
    let x' = pack x
        y' = pack y in
    editDistance b x' y' == editDistance b y' x'

prop_dist_greater_than_0 x y b
    | x == y    = True
    | otherwise =
        let x' = pack x
            y' = pack y in
        editDistance b x' y' > 0

main :: IO ()
main = $defaultMainGenerator

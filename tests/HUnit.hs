{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main (HUnit)
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Main where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit (assertEqual)

import Data.Spellcheck.Distance

testDataNT :: [(Text, Text, Int)]
testDataNT =
    [("sitting", "kitten", 3)
    ,("an act", "a cat", 3)
    ,("sunday", "saturday", 3)
    ]

testDataT :: [(Text, Text, Int)]
testDataT =
    [("an act", "a cat", 2)]

case_dist_non_trans = runTestCases False testDataNT

case_dist_trans = runTestCases True testDataT

runTestCases b xs =
    forM_ xs $ \(s1,s2,exp) ->
        let exp_str = show exp
            str     = show (s1,s2) ++ "should equal to " ++ exp_str in
        assertEqual exp_str  exp (editDistance b s1 s2)

main :: IO ()
main = $defaultMainGenerator

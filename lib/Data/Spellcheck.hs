{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
----------------------------------------------------------------------------
module Data.Spellcheck where

import Data.Text(Text)

import Data.Spellcheck.HolbrookCorpus
import Data.Spellcheck.LanguageModel
import Data.Spellcheck.SpellingResult

data Spellcheck m where
    Spellcheck :: LanguageModel m => m -> Spellcheck m

evaluate :: Spellcheck m -> HolbrookCorpus -> IO SpellingResult
evaluate = undefined

correctSentence :: Spellcheck m -> [Text] -> IO [Text]
correctSentence = undefined

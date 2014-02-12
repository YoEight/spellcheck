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

import Data.Spellcheck.EditModel
import Data.Spellcheck.HolbrookCorpus
import Data.Spellcheck.LanguageModel
import Data.Spellcheck.SpellingResult

data Spellcheck m where
    Spellcheck :: LanguageModel m => m -> EditModel -> Spellcheck m

mkSpellcheck :: LanguageModel m => m -> IO (Spellcheck m)
mkSpellcheck m = do
    corpus <- trainCorpus
    editm  <- mkEditModel corpus
    return $ Spellcheck m editm

mkSpellcheckWithCorpus :: LanguageModel m
                       => m
                       -> HolbrookCorpus
                       -> IO (Spellcheck m)
mkSpellcheckWithCorpus m corpus = fmap (Spellcheck m) (mkEditModel corpus)

evaluate :: Spellcheck m -> HolbrookCorpus -> IO SpellingResult
evaluate = undefined

correctSentence :: Spellcheck m -> [Text] -> IO [Text]
correctSentence = undefined

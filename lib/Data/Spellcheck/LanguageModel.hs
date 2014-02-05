-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.LanguageModel
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- returns score for a sentence. (list of words) trains on corpus
----------------------------------------------------------------------------
module Data.Spellcheck.LanguageModel where

import Data.Text (Text)

import Data.Spellcheck.HolbrookCorpus

class LanguageModel a where
    train :: HolbrookCorpus -> IO a

    -- | Language model score (probality) of given sentence
    --   usually a log-probability
    score :: a -> [Text] -> Double

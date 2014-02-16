-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
import Data.Spellcheck
import Data.Spellcheck.HolbrookCorpus
import Data.Spellcheck.LanguageModel
import Data.Spellcheck.SpellingResult
import Data.Spellcheck.UniformLanguageModel

main :: IO ()
main = do
    tc <- trainCorpus
    dc <- devCorpus
    print "-- Uniform Language Model --"
    um  <- train tc :: IO UniformLanguageModel
    usc <- mkSpellcheckWithCorpus um tc
    res <- evaluate usc dc
    print $ spellingAccuracy res

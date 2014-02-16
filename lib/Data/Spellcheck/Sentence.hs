-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.Sentence
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Spellcheck.Sentence
       ( Sentence
       , SentenceToken(..)
       , sentenceHasError
       , sentenceIsCorrection
       , sentenceGetCorrect
       , sentenceGetWrong
       ) where

import Data.Maybe (fromMaybe)

import Data.Text (Text)

import Data.Spellcheck.Datum

data SentenceToken = SStart
                   | SDatum !Datum
                   | SEnd deriving Show

type Sentence = [SentenceToken]

sentenceDatums :: Sentence -> [Datum]
sentenceDatums xs = go =<< xs
  where
    go (SDatum d) = [d]
    go _          = []

sentenceHasError :: Sentence -> Bool
sentenceHasError = go . sentenceDatums
  where
    go = any $ \d ->  datumHasError d && datumValid d

sentenceIsCorrection :: Sentence -> [Text] -> Bool
sentenceIsCorrection stc cand
    | length stc /= length cand = False
    | otherwise =
        let xs = zip (sentenceGetCorrect stc) cand in
        all (uncurry (==)) xs

-- | Returns the corrected sentence
sentenceGetCorrect :: Sentence -> [Text]
sentenceGetCorrect = fmap datumWord . sentenceDatums

-- | Returns the sentence as written, flattened into list of Text
sentenceGetWrong :: Sentence -> [Text]
sentenceGetWrong = fmap go . sentenceDatums
  where
    go d = fromMaybe (datumWord d) (datumError d)

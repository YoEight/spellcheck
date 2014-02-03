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
       ) where

import Data.Spellcheck.Datum (Datum)

data SentenceToken = SStart
                   | SDatum !Datum
                   | SEnd deriving Show

type Sentence = [SentenceToken]
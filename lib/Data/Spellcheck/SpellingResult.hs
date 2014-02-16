-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.SpellingResult
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
----------------------------------------------------------------------------
module Data.Spellcheck.SpellingResult where

data SpellingResult
    = SpellingResult
      { srCorrect :: {-# UNPACK #-} !Int
      , srTotal   :: {-# UNPACK #-} !Int
      }

spellingAccuracy :: SpellingResult -> Double
spellingAccuracy (SpellingResult c t)
    | t == 0    = 0
    | otherwise = fromIntegral c / fromIntegral t

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.Datum
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Spellcheck.Datum
       ( Datum
       , datum, datumCorrect, datumWord, datumError
       , datumHasError
       , datumSetError
       , datumValid
       ) where

import Data.Char (isLetter)
import Data.Maybe (isJust)

import Data.Spellcheck.Distance (editDistance)

import qualified Data.Text as T

-- | The word is the corrected word, and error contains the spelling error
data Datum = Datum { datumWord  :: T.Text
                   , datumError :: Maybe T.Text
                   }

instance Show Datum where
    show d@(Datum w e) = "word:\"" ++ show w ++ "\" error: " ++
                       show e ++ " dist: " ++ show dist ++
                       " valid: " ++ show valid
      where
        dist  = fmap (editDistance True w) e
        valid = datumValid d

datum :: T.Text -> Maybe T.Text -> Datum
datum = Datum

datumCorrect :: T.Text -> Datum
datumCorrect w = datum w Nothing

datumHasError :: Datum -> Bool
datumHasError = isJust . datumError

datumSetError :: Maybe T.Text -> Datum -> Datum
datumSetError e s = s{ datumError =  e }

-- | Returns True if the error is within edit distance 1 and contains
--   no numerics/punctuation
datumValid :: Datum -> Bool
datumValid d =
    case datumError d of
        Nothing -> False
        Just e  ->
            let w        = datumWord d
                dist     = editDistance True w e
                is_clean = T.all isLetter w && T.all isLetter e in
            dist <= 1 || is_clean

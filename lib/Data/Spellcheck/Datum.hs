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
       ( datum, datumCorrect, datumWord, datumError
       , datumHasError
       , datumSetError
       ) where

import Data.Maybe (isJust)

import qualified Data.Text as T

-- | The word is the corrected word, and error contains the spelling error
data Datum = Datum { datumWord  :: T.Text
                   , datumError :: Maybe T.Text
                   }

instance Show Datum where
    show (Datum w e) = "word:\"" ++ show w ++ "\" error: " ++ show e

datum :: T.Text -> Maybe T.Text -> Datum
datum = Datum

datumCorrect :: T.Text -> Datum
datumCorrect w = datum w Nothing

datumHasError :: Datum -> Bool
datumHasError = isJust . datumError

datumSetError :: Maybe T.Text -> Datum -> Datum
datumSetError e s = s{ datumError =  e }

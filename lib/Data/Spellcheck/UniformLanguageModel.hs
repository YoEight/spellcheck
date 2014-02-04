-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.UniformLanguageModel
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- A uniform language model. This simply counts the vocabulary size V of the
-- training corpus and assigns p(w) = 1/V for any word.
----------------------------------------------------------------------------
module Data.Spellcheck.UniformLanguageModel
       ( UniformLanguageModel
       , train
       , score
       ) where

import           Data.Foldable (foldMap)
import           Data.Monoid
import qualified Data.Set as S

import qualified Data.Text as T

import Data.Spellcheck.Datum
import Data.Spellcheck.HolbrookCorpus
import Data.Spellcheck.Sentence

data UniformLanguageModel = ULM !(S.Set T.Text)

instance Monoid UniformLanguageModel where
    mempty = ULM S.empty
    mappend (ULM e) (ULM e') = ULM (S.union e e')

train :: FilePath -> IO UniformLanguageModel
train filepath = do
    xs <- loadHolbrook filepath
    let model = foldMap (foldMap go) xs
    return model
  where
    go (SDatum d) = ULM $ S.singleton (datumWord d)
    go _          = ULM S.empty

score :: UniformLanguageModel -> [T.Text] -> Double
score (ULM s) stc = (fromIntegral $ length stc) * log (fromIntegral $ S.size s)
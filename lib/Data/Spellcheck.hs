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

import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST, runST)
import           Data.Foldable (foldMap, traverse_)
import qualified Data.Map.Strict as M
import           Data.Monoid (Monoid(..))

import           Control.Monad.State.Strict
import           Data.Text(Text)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import Data.Spellcheck.EditModel
import Data.Spellcheck.HolbrookCorpus
import Data.Spellcheck.LanguageModel
import Data.Spellcheck.Sentence
import Data.Spellcheck.SpellingResult

data Spellcheck m where
    Spellcheck :: LanguageModel m => m -> EditModel -> Spellcheck m

data Evaluate
    = Evaluate
      { evalTotal   :: {-# UNPACK #-} !Int
      , evalCorrect :: {-# UNPACK #-} !Int
      }

instance Monoid Evaluate where
    mempty = Evaluate 0 0

    mappend (Evaluate x1 y1) (Evaluate x2 y2) =
        Evaluate (x1+x2) (y1+y2)

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
evaluate sc = fmap (eval . foldMap go) . corpusTestCases
  where
    one  = Evaluate 1 1
    zero = Evaluate 1 0
    none = Evaluate 0 0

    eval (Evaluate total correct) =
        SpellingResult correct total

    go []  = none
    go stc =
        let wrongSentence = sentenceGetWrong stc
            hypothesis    = correctSentence sc wrongSentence in
        if sentenceIsCorrection stc hypothesis
        then one
        else zero

data CorrectState
    = CS
      { cs_arg_max_idx :: {-# UNPACK #-} !Int
      , cs_arg_max_wd  :: !Text
      , cs_max         :: {-# UNPACK #-} !Double
      , cs_max_lm      :: {-# UNPACK #-} !Double
      , cs_max_edit    :: {-# UNPACK #-} !Double
      }

initCorrectState :: Text -> CorrectState
initCorrectState x =
    CS{ cs_arg_max_idx = 0
      , cs_arg_max_wd  = x
      , cs_max         = neg_inf
      , cs_max_lm      = neg_inf
      , cs_max_edit    = neg_inf
      }
  where
    neg_inf = (-10000000)

correctSentence :: Spellcheck m -> [Text] -> [Text]
correctSentence _ [] = []
correctSentence (Spellcheck model edit_model) stc@(x:xs) =
    corrected
  where
    indexes    = [1..len-1]
    init_state = initCorrectState x
    len        = length stc
    vec = V.create $ flip evalStateT 0 $ do
        v <- lift $ VM.new len
        forM_ stc $ \s -> do
            i <- get
            lift $ VM.write v i s
            put (i+1)
        return v

    corrected =
        let cs       = execState action init_state
            max_idx  = cs_arg_max_idx cs
            max_word = cs_arg_max_wd cs in
        updateList (max_idx, max_word) stc

    action = traverse_ state_action (zip indexes xs)

    state_action (i,word) =
        let probs = editProbabilities edit_model word
            alts  = M.keys probs in
        forM_ alts $ \alt ->
        when (alt /= word) $ do
            cs <- get
            let lmscore   = score model (vec V.// [(i,alt)])
                editscore = log (M.findWithDefault 0 alt probs)
                tmp_score = lmscore + editscore
            when (tmp_score >= cs_max cs) $
                put cs{ cs_max         = tmp_score
                      , cs_max_lm      = lmscore
                      , cs_max_edit    = editscore
                      , cs_arg_max_idx = i
                      , cs_arg_max_wd  = alt
                      }

updateList :: (Int, a) -> [a] -> [a]
updateList (idx,v) xs = go 0 xs
  where
    go i xs | i == idx = v : xs
    go _ []            = []
    go i (x:xs)        = x : go (i+1) xs

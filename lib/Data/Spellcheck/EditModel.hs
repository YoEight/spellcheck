-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.EditModel
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Spellcheck.EditModel
       ( EditModel(..), Probabilities
       , mkEditModel
       , loadEdits
       , editCount
       , editProbabilities
       ) where

import           Prelude hiding (lines, takeWhile)
import           Data.Char
import           Data.Foldable (foldMap, traverse_)
import           Data.List (foldl')
import           Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.Text
import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.Text (decode, iso8859_1, lines)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Data.Spellcheck.Datum
import Data.Spellcheck.HolbrookCorpus
import Data.Spellcheck.Sentence
import Data.Spellcheck.Utils

data EditModel
    = EditModel
      { emEditCount  :: !(M.Map T.Text Int)
      , emVocabulary :: !(S.Set T.Text)
      }

type Probabilities = M.Map T.Text Double

mkEditModel :: HolbrookCorpus -> IO EditModel
mkEditModel corpus = do
    editCount <- loadEdits
    xs        <- corpusLoad corpus
    let vocab = foldMap (foldMap selection) xs
    return $ EditModel editCount vocab
  where
    selection (SDatum d) = S.singleton $ datumWord d
    selection _          = S.empty

loadEdits :: IO (M.Map T.Text Int)
loadEdits = runResourceT processEdit
  where
    toText = decode iso8859_1

    source = sourceFile "data/count_1edit.txt"

    processEdit = source $= toText =$= lines $$ registerEdits M.empty

-- | Returns how many times substring s1 is edited as s2.
--   For example editCount(e,i) counts how many times the correct 'i' is
--   misspelled as an 'e'
editCount :: EditModel -> T.Text -> T.Text -> Int
editCount em s1 s2 = fromMaybe 0 (M.lookup key $ emEditCount em)
  where
    key = T.append s1 $ T.append (T.pack "|") s2

alphabet :: String
alphabet = ['a'..'z']

-- | Computes p(x| word) for x in the dictionary within edit distance one of
--   word.
editProbabilities :: EditModel -> T.Text -> Probabilities
editProbabilities em word = probs
  where
    w_inits = T.inits word
    w_tails = T.tails word
    vocab   = emVocabulary em
    xs      = zip w_inits w_tails
    counts  = execState (traverse_ pop_counts xs) M.empty
    total   = foldl' (+) 0 (M.elems counts)
    self_count = (9*total) `max` 1
    norm_counts = M.insert word self_count counts
    norm_total  = total + self_count
    probs = fmap (\i -> fromIntegral i / fromIntegral norm_total) norm_counts

    counts_del a b =
        when (S.member deleted vocab) $ do
            m <- get
            let prev_count = fromMaybe 0 (M.lookup deleted m)
            put (M.insert deleted (del_edit_count + prev_count) m)
      where
         deleted = T.append a (T.drop 1 b)
         del_tail
             | T.length a > 0 = T.pack [T.last a]
             | otherwise      = T.empty
         del_orig = T.append del_tail (T.pack [T.head b])
         del_edit_count = editCount em del_orig del_tail

    counts_trans a b =
        when (S.member transposed vocab) $ do
            m <- get
            let prev_count = fromMaybe 0 (M.lookup transposed m)
            put (M.insert transposed (trans_count + prev_count) m)
      where
        transposed = T.append a
                     (T.append (T.pack [T.index b 1, T.head b]) (T.drop 2 b))
        trans_orig  = T.take 2 b
        trans_repl  = T.reverse trans_orig
        trans_count = editCount em trans_orig trans_repl

    counts_repl a b =
        forM_ alphabet $ \c ->
        let replaced = T.append a
                       (T.cons c (T.drop 1 b)) in
        when (S.member replaced vocab) $ do
            m <- get
            let repl_orig  = T.pack [T.head b]
                repl       = T.pack [c]
                repl_count = editCount em repl_orig repl
                prev_count = fromMaybe 0 (M.lookup replaced m)
            put (M.insert replaced (repl_count + prev_count) m)

    counts_insert a b =
        forM_ alphabet $ \c ->
        let inserted  = T.append a (T.cons c b) in
        when (S.member inserted vocab) $ do
            m <- get
            let ins_tail   = if T.null a then T.empty else T.pack [T.last a]
                ins_repl   = T.append ins_tail (T.pack [c])
                ins_count  = editCount em ins_tail ins_repl
                prev_count = fromMaybe 0 (M.lookup inserted m)
            put (M.insert inserted (ins_count + prev_count) m)

    go = undefined

    pop_counts (a,b) = do
        when (T.length b > 0) (counts_del a b >> counts_repl a b)
        when (T.length b > 1) (counts_trans a b)
        counts_insert a b

registerEdits :: M.Map T.Text Int
              -> Sink T.Text (ResourceT IO) (M.Map T.Text Int)
registerEdits m = maybe (return m) go =<< await
  where
    go t = do
        m' <- registerEdit t
        registerEdits m'

    registerEdit t =
        case parseOnly parser t of
            Left e        -> monadThrow $ PE e
            Right (ed, v) -> return $ M.insert ed v m

parser :: Parser (T.Text, Int)
parser = do
    t <- takeWhile1 (/= '\t')
    anyChar
    v <- decimal
    endOfInput
    return (t, v)

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
       ( EditModel(..)
       , mkEditModel
       , loadEdits
       , editCount
       )where

import           Prelude hiding (lines, takeWhile)
import           Data.Char
import           Data.Foldable (foldMap)
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

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

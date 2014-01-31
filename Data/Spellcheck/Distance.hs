-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.Distance
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Spellcheck.Distance (editDistance) where

import Control.Applicative (Applicative, pure, (*>), (<$))
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (StateT, gets, evalStateT, modify, lift)
import Data.Maybe (fromMaybe)

import qualified Data.Text                   as T
import qualified Data.Vector.Unboxed.Mutable as V

-- | Datatype used during edit distance computation
data EditState s = EditState
                   { lastSlice    :: !(V.MVector s Int)
                   , currentSlice :: !(V.MVector s Int)
                   , editX        :: {-# UNPACK #-} !Int
                   , editY        :: {-# UNPACK #-} !Int
                   }

type SlicesAndX s = (V.MVector s Int, V.MVector s Int, Int)

-- | Returns the edit distance between the character sequences with
--   or without transpositions as specified.  This distance is symmetric
--   It's a Levenshtein distance, implemented by an optimized Wagner–Fischer
--   algorithm
editDistance :: Bool   -- allow transposition ?
             -> T.Text
             -> T.Text
             -> Int
editDistance allow seq1 seq2
    | T.length seq2 == 0            = T.length seq1
    | T.length seq2 == 1            = editDistanceSmall seq1 seq2
    | T.length seq1 < T.length seq2 = editDistance allow seq2 seq1
    | allow                         = editDistanceTranspose seq1 seq2
    | otherwise                     = editDistanceNonTranspose seq1 seq2

editDistanceSmall :: T.Text -> T.Text -> Int
editDistanceSmall seq1 seq2 = fromMaybe seq1_length res
  where
    seq1_length = T.length seq1
    seq2_head   = T.head seq2
    whenFound   = T.find (== seq2_head) seq1
    res         = T.length seq1 - 1 <$ whenFound

editDistanceNonTranspose :: T.Text -> T.Text -> Int
editDistanceNonTranspose seq1 seq2 = runST action
  where
    seq2_length   = T.length seq2
    vector_length = seq2_length + 1

    action = do
        lastS <- V.new vector_length
        curS  <- V.new vector_length
        let init_state = EditState lastS curS 1 1
        init_vector curS
        evalStateT go init_state

    init_vector vec =
        forM_ [0..vector_length-1] $ \i ->
            V.write vec i i

    go :: StateT (EditState s) (ST s) Int
    go = do
        textFor_ seq1 $ \c_x -> do
            editSwitchSlices
            (lastS, curS, x) <- editGetSlicesAndX
            lift $ V.write curS 0 x
            editSetY 1
            textFor_ seq2 $ \c_y -> do
                y    <- gets editY
                cost <- get_cost lastS curS c_x c_y y
                lift $ V.write curS y cost
                editIncrY
            editIncrX
        curS <- gets currentSlice
        let last_idx = V.length curS - 1
        lift $ V.read curS last_idx

    get_cost lastS curS c_x c_y y
        | c_x == c_y = lift $ V.read lastS (y-1)
        | otherwise  = lift $ do
            subst_val <- V.read lastS (y-1)
            del_val   <- V.read lastS y
            ins_val   <- V.read curS (y-1)
            let min_val = min subst_val $ min del_val ins_val
            return (1+min_val)

editDistanceTranspose :: T.Text -> T.Text -> Int
editDistanceTranspose = error "not implemented yet"

textFor_ :: Applicative m => T.Text -> (Char -> m a) -> m ()
textFor_ txt k
    | Just (x, xs) <- T.uncons txt = k x *> textFor_ xs k
    | otherwise                    = pure ()

editSwitchSlices :: Monad m => StateT (EditState s) m ()
editSwitchSlices = modify go
  where
    go s@EditState{ lastSlice=l, currentSlice=c } =
        s{ lastSlice=c, currentSlice=l }

editGetSlicesAndX :: Monad m => StateT (EditState s) m (SlicesAndX s)
editGetSlicesAndX = gets go
  where
    go s =
        let lastS = lastSlice s
            curS  = currentSlice s
            x     = editX s in
        (lastS, curS, x)

editIncrX :: Monad m => StateT (EditState s) m ()
editIncrX = modify go
  where
    go s@EditState{ editX=x } = s{ editX=x+1 }

editIncrY :: Monad m => StateT (EditState s) m ()
editIncrY = modify go
  where
    go s@EditState{ editY=y } = s{ editY=y+1 }

editSetY :: Monad m => Int -> StateT (EditState s) m ()
editSetY y = modify go
  where
    go s = s{ editY=y }

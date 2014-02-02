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
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (StateT, gets, evalStateT, modify, lift)
import Data.Maybe (fromMaybe)

import qualified Data.Text                   as T
import qualified Data.Vector.Unboxed.Mutable as V

-- | Datatype used during edit distance computation
data EditState s
    = EditState
      { lastSlice    :: !(V.MVector s Int)
      , currentSlice :: !(V.MVector s Int)
      , editX        :: {-# UNPACK #-} !Int
      , editY        :: {-# UNPACK #-} !Int
      }
    | EditTransState
      { lastSlice    :: !(V.MVector s Int)
      , currentSlice :: !(V.MVector s Int)
      , twoLastSlice :: !(V.MVector s Int)
      , editX        :: {-# UNPACK #-} !Int
      , editY        :: {-# UNPACK #-} !Int
      , lastXChar    :: {-# UNPACK #-} !Char
      , lastYChar    :: {-# UNPACK #-} !Char
      }

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
        editInitVectorTo seq2_length curS
        evalStateT go init_state

    go :: StateT (EditState s) (ST s) Int
    go = do
        textFor_ seq1 $ \c_x -> do
            editSwitchSlices
            curS <- gets currentSlice
            x    <- gets editX
            lift $ V.write curS 0 x
            editSetY 1
            textFor_ seq2 $ \c_y -> do
                y    <- gets editY
                cost <- editGetCost y c_x c_y
                lift $ V.write curS y cost
                editIncrY
            editIncrX
        curS <- gets currentSlice
        let last_idx = V.length curS - 1
        lift $ V.read curS last_idx

editDistanceTranspose :: T.Text -> T.Text -> Int
editDistanceTranspose seq1 seq2 = runST action
  where
    seq2_length   = T.length seq2
    vector_length = seq2_length + 1
    c_x0          = T.head seq1
    c_y0          = T.head seq2

    action = do
        lastS    <- V.new vector_length
        curS     <- V.new vector_length
        twolastS <- V.new vector_length
        let init_state = EditTransState lastS curS twolastS 1 1 '?' '?'
        editInitVectorTo seq2_length lastS
        V.write curS 0 1
        evalStateT go init_state

    go :: StateT (EditState s) (ST s) Int
    go = do
        textFor_ seq2 $ \c_y -> do
            y    <- gets editY
            curS <- gets currentSlice
            cost <- editGetCost y c_x0 c_y
            lift $ V.write curS y cost
            editIncrY
        editSetX 2
        editSetLastXChar c_x0
        textFor_ (T.tail seq1) $ \c_x -> do
            editSwitchTransSlices
            x      <- gets editX
            curS   <- gets currentSlice
            lift $ V.write curS 0 x
            cost_x <- editGetCost 1 c_x c_y0
            lift $ V.write curS 1 cost_x
            editSetY 2
            editSetLastYChar c_y0
            textFor_ (T.tail seq2) $ \c_y -> do
                y      <- gets editY
                m_x    <- gets lastXChar
                m_y    <- gets lastYChar
                cost_y <- editGetCost y c_x c_y
                lift $ V.write curS y cost_y
                when (c_x == m_y && c_y == m_x) $ do
                    last2S    <- gets twoLastSlice
                    last2_val <- lift $ V.read last2S (y-2)
                    lift $ V.write curS y (min cost_y (last2_val+1))
                editSetLastYChar c_y
                editIncrY
            editSetLastXChar c_x
            editIncrX
        curS <- gets currentSlice
        lift $ V.read curS seq2_length

textFor_ :: Applicative m => T.Text -> (Char -> m a) -> m ()
textFor_ txt k
    | Just (x, xs) <- T.uncons txt = k x *> textFor_ xs k
    | otherwise                    = pure ()

editSwitchSlices :: Monad m => StateT (EditState s) m ()
editSwitchSlices = modify go
  where
    go s =
        let l = lastSlice s
            c = currentSlice s in
        s{ lastSlice=c, currentSlice=l }

editSwitchTransSlices :: Monad m => StateT (EditState s) m ()
editSwitchTransSlices = modify go
  where
    go s =
        let l  = lastSlice s
            l2 = twoLastSlice s
            c  = currentSlice s in
        s{ lastSlice=c, twoLastSlice=l, currentSlice=l2 }

editIncrX :: Monad m => StateT (EditState s) m ()
editIncrX = modify go
  where
    go s = s{ editX=editX s+1 }

editIncrY :: Monad m => StateT (EditState s) m ()
editIncrY = modify go
  where
    go s = s{ editY=editY s+1 }

editSetY :: Monad m => Int -> StateT (EditState s) m ()
editSetY y = modify go
  where
    go s = s{ editY=y }

editSetX :: Monad m => Int -> StateT (EditState s) m ()
editSetX x = modify go
  where
    go s = s{ editX=x }

editSetLastXChar :: Monad m => Char -> StateT (EditState s) m ()
editSetLastXChar c = modify go
  where
    go s = s{ lastXChar=c }

editSetLastYChar :: Monad m => Char -> StateT (EditState s) m ()
editSetLastYChar c = modify go
  where
    go s = s{ lastYChar=c }

editInitVectorTo :: Int -> V.MVector s Int -> ST s ()
editInitVectorTo rmax vec =
    forM_ [0..rmax] $ \i ->
        V.write vec i i

editGetCost :: Int -> Char -> Char -> StateT (EditState s) (ST s) Int
editGetCost i c_x c_y = do
    lastS <- gets lastSlice
    curS  <- gets currentSlice
    if c_x == c_y
        then lift $ V.read lastS (i-1)
        else lift $ do
        subst_val <- V.read lastS (i-1)
        del_val   <- V.read lastS i
        ins_val   <- V.read curS (i-1)
        let min_val = min subst_val $ min del_val ins_val
        return (1+min_val)

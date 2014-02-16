{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.HolbrookCorpus
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Data.Spellcheck.HolbrookCorpus
       ( HolbrookCorpus
       , corpusLoad
       , mkCorpus
       , loadHolbrook
       , trainCorpus
       , devCorpus
       , corpusTestCases
       ) where

import Prelude hiding (lines, takeWhile)
import Data.Char (isAlphaNum, isPunctuation, isSpace)
import Data.IORef
import Data.Maybe (fromMaybe)

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad (mzero, mplus)
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.Text
import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.Text (decode, utf8, lines)
import qualified Data.Text as T

import Data.Spellcheck.Datum
import Data.Spellcheck.Sentence
import Data.Spellcheck.Utils

newtype HolbrookCorpus = HC (IO [Sentence])

mkCorpus :: FilePath -> IO HolbrookCorpus
mkCorpus filepath = fmap go (newIORef Nothing)
  where
    go ref = HC $ do
        v <- readIORef ref
        case v of
            Just xs -> return xs
            _       -> do
                xs <- loadHolbrook filepath
                writeIORef ref (Just xs)
                return xs

trainCorpus, devCorpus :: IO HolbrookCorpus
trainCorpus = mkCorpus "data/holbrook-tagged-train.dat"
devCorpus   = mkCorpus "data/holbrook-tagged-dev.dat"

corpusLoad :: HolbrookCorpus -> IO [Sentence]
corpusLoad (HC run) = run

corpusTestCases :: HolbrookCorpus -> IO [Sentence]
corpusTestCases = fmap go . corpusLoad
  where
    go = filter sentenceHasError

-- | Load HolbrookCorpus format corpus into list of Sentence
loadHolbrook :: FilePath -> IO [Sentence]
loadHolbrook filepath = runResourceT process
  where
    toText = decode utf8

    source = sourceFile filepath

    process = source $= toText =$= lines $$ mkSentences

mkSentences :: Sink T.Text (ResourceT IO) [Sentence]
mkSentences = maybe (return []) go =<< await
  where
    go t = (:) <$> mkSentence t <*> mkSentences

    mkSentence =
        either (monadThrow . PE) return . parseOnly parser

parser :: Parser Sentence
parser = takeWhile isAlphaNum >>= go
  where
    go xs
        | T.null xs = do
            end <- atEnd
            if end then return [SEnd] else peekChar' >>= loop
        | otherwise = do
            cOpt <- peekChar
            case cOpt of
                Nothing -> return [mkDatum xs, SEnd]
                Just c  -> fmap (mkDatum xs:) (loop c)

    loop c
        | isBreaker c = anyChar >> parser
        | c == '<'    = (:) <$> parseError <*> parser
        | otherwise   = fail ("Unexpected " ++ [c])

    isBreaker c =
        isSpace c || isPunctuation c || c == '`'

    mkDatum xs = SDatum $ datumCorrect $ T.toLower xs

parseError :: Parser SentenceToken
parseError = do
    word <- "<ERR targ=" .*> takeWhile1 (/= '>')
    char '>'
    err  <- takeWhile1 (/= '<')
    string "</ERR>" `mplus` fail "Not ended by </ERR>"
    let dat = datum word (Just err)
    return $ SDatum dat

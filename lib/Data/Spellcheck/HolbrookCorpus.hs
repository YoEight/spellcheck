{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
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
       ) where

import Prelude hiding (lines, takeWhile)
import Control.Exception.Base
import Data.Char (isAlphaNum, isPunctuation, isSpace)
import Data.Maybe (fromMaybe)
import Data.Typeable

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

newtype HolbrookCorpus = HC (IO [Sentence])

data ParseException = PE !String deriving (Show, Typeable)

instance Exception ParseException

mkCorpus :: FilePath -> HolbrookCorpus
mkCorpus filepath = HC (loadHolbrook filepath)

corpusLoad :: HolbrookCorpus -> IO [Sentence]
corpusLoad (HC run) = run

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

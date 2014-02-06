{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Spellcheck.Utils
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Spellcheck.Utils where

import Control.Exception.Base
import Data.Typeable

data ParseException = PE !String deriving (Show, Typeable)

instance Exception ParseException

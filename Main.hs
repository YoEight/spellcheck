-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------

import Data.Char
import Data.String
import System.Environment

import qualified Data.Spellcheck.UniformLanguageModel as U

main :: IO ()
main = undefined
    -- (opt, ws) <- parseInput
    -- case opt of
    --     "-uni" -> do
    --         ulm <- U.train "data/holbrook-tagged-train.dat"
    --         let score = U.score ulm (fmap (fromString . fmap toLower) ws)
    --         print score
    --     _ -> ioError $ userError ("Unsupported option: " ++ opt)

parseInput :: IO (String, [String])
parseInput = getArgs >>= go
  where
    go (opt:ws) = return (opt, ws)
    go _        = ioError $ userError "Bad use"

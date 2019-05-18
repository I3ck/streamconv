{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Types
import Instances ()
import Args
import Run
import System.IO
import Options.Applicative
import System.Exit
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.IO as LIO

--------------------------------------------------------------------------------

--- TODO rename
--- TODO strong types
--- TODO just accepts ArgsRaw?
readEnvironment :: Handle -> String -> String -> String -> T.Text -> T.Text -> IO Environment
readEnvironment h p ptmp1 ptmp2 delimVal delimLine = do
  ba1 <- LIO.readFile p
  ba2 <- LIO.readFile p
  bb1 <- BL.readFile p
  bb2 <- BL.readFile p
  pure Environment{ eBlobA1 = ba1, eBlobA2 = ba2, eBlobB1 = bb1, eBlobB2 = bb2, eXyzVal = delimVal, eXyzLine = delimLine, eHandle = h, eTmp1 = ptmp1, eTmp2 = ptmp2 }

main :: IO ()
main = do
  rargs <- execParser opts
  case createArgs rargs of
    Left e         -> die e
    Right Args{..} -> withFile pOut WriteMode (\h -> do
        env <- readEnvironment h pIn tmp1 tmp2 ";" "\n"
        when list $ putStrLn $ showCombinations $ combinations env
        run env fIn fOut)
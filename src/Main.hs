{-# LANGUAGE RecordWildCards   #-}

module Main where

import Types
import Instances ()
import Args
import Run

import System.IO
import Control.Monad
import qualified Options.Applicative as OA
import qualified System.Exit as E
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.IO as LIO

--------------------------------------------------------------------------------

main :: IO ()
main = do
  rargs <- OA.execParser opts
  case createArgs rargs of
    Left e         -> E.die e
    Right a@Args{..} -> withFile pOut WriteMode (\h -> do
        env <- createEnvironment h a
        when list $ putStrLn $ showCombinations $ combinations env
        run env fIn fOut)

--------------------------------------------------------------------------------

createEnvironment :: Handle -> Args -> IO Environment
createEnvironment h Args{..} = do
  ba1 <- LIO.readFile pIn
  ba2 <- LIO.readFile pIn
  bb1 <- BL.readFile pIn
  bb2 <- BL.readFile pIn
  pure Environment
    { eBlobA1  = ba1
    , eBlobA2  = ba2
    , eBlobB1  = bb1
    , eBlobB2  = bb2
    , eXyzVal  = T.pack xyzVal
    , eXyzLine = T.pack xyzLine
    , eHandle  = h
    , eTmp1    = tmp1
    , eTmp2    = tmp2 
    }

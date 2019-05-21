{-# LANGUAGE RecordWildCards #-}

module Args
  ( opts
  , createArgs
  ) where

import Constants
import Types
import Utils

import Data.List
import Options.Applicative

--------------------------------------------------------------------------------

createArgs :: ArgsRaw -> Either String Args ---TODO error types
createArgs ArgsRaw{..} = do
  fIn  <- maybeToEither "Unknown input format"  $ maybeRead rfIn
  fOut <- maybeToEither "Unknown output format" $ maybeRead rfOut
  pure Args{pIn = rpIn, pOut = rpOut, fIn = fIn, fOut = fOut, tmp1 = rTmp1, tmp2 = rTmp2, xyzVal = rXyzVal, xyzLine = rXyzLine, list = rList}

--------------------------------------------------------------------------------

opts :: ParserInfo ArgsRaw
opts = info (helper <*> args)
  ( fullDesc
  <> progDesc ("streamconv " ++ version ++" - Memory efficient conversion between pointcloud formats. Use --help for more information (c) Martin Buck")
  <> header ("streamconv " ++ version ++ " (c) Martin Buck")
  )

--------------------------------------------------------------------------------

args :: Parser ArgsRaw
args = ArgsRaw
  <$> strOption
    ( long "pin"
    <> help "Path to the input file"
    <> metavar "STRING"
    )
  <*> strOption
    ( long "pout"
    <> help "Path to write to"
    <> metavar "STRING"
    )
  <*> strOption
    ( long "fin"
    <> help ("Input format" ++ sFormats)
    <> metavar "STRING"
    )
  <*> strOption
    ( long "fout"
    <> help ("Output format" ++ sFormats)
    <> metavar "STRING"
    )
  <*> strOption
    ( long "tmp1"
    <> help "Path that shall be used to write temporary data"
    <> metavar "STRING"
    <> value "streamconvtmp1.tmp"
    <> showDefault
    )
  <*> strOption
    ( long "tmp2"
    <> help "Path that shall be used to write temporary data"
    <> metavar "STRING"
    <> value "streamconvtmp2.tmp"
    <> showDefault
    )
  <*> strOption
    ( long "xyzval"
    <> help "Delimiter to be used between xyz values"
    <> metavar "STRING"
    <> value ";"
    <> showDefault
    )
  <*> strOption
    ( long "xyzline"
    <> help "Delimiter to be used between xyz lines"
    <> metavar "STRING"
    <> value "\n"
    <> showDefault
    )
  <*> switch
    ( long "list"
    <> help "List available combinations"
    )
  where
    sFormats = " [" ++ intercalate ", " (show <$> formats) ++ " ]"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Conduit
import Types
import Instances ()
import Sources
import Transformers
import Sinks
import System.IO
import Options.Applicative
import Data.List
import System.Exit
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Maybe as M
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as LIO

--------------------------------------------------------------------------------

bufferSize = 100

--------------------------------------------------------------------------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = valueIfNoRemainder =<< (M.listToMaybe . reads $ s)
  where
    valueIfNoRemainder (x, rem) | null rem  = Just x
                                | otherwise = Nothing

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _   (Just x) = Right x
maybeToEither err Nothing  = Left err
                                

createArgs :: ArgsRaw -> Either String Args ---TODO error types
createArgs ArgsRaw{..} = do
  fIn  <- maybeToEither "Unknown input format"  $ maybeRead rfIn
  fOut <- maybeToEither "Unknown output format" $ maybeRead rfOut
  pure Args{pIn = rpIn, pOut = rpOut, fIn = fIn, fOut = fOut, list = rList}

opts :: ParserInfo ArgsRaw
opts = info (helper <*> args)
  ( fullDesc
  <> progDesc "streamconv 0.0.1 - Memory efficient conversion between pointcloud formats. Use --help for more information (c) Martin Buck"
  <> header "streamconv 0.0.1 (c) Martin Buck"
  )

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
  <*> switch
    ( long "list"
    <> help "List available combinations"
    )
  where
    sFormats = " [" ++ intercalate ", " (show <$> formats) ++ " ]"

--------------------------------------------------------------------------------

--- TODO consider passing Args here
--- TODO expand to support all possible combinations (for now, later consider better abstraction to not have quadratic complexity)
run :: Environment -> Format -> Format -> IO ()
run env from to = case resolve env from to of
  Nothing -> putStrLn $ "Conversion from " ++ show from ++ " to " ++ show to ++ " not supported (yet)" ---TODO more info
  Just x  -> x

resolve :: Environment -> Format -> Format -> Maybe (IO ())
resolve env from to = result
  where 
    pfSource      = M.lookup from pfSources
    tripletSource = M.lookup from tripletSources
    posSource     = M.lookup from posSources
    xyzSink       = M.lookup to   xyzSinks
    xySink        = M.lookup to   xySinks
    tripletSink   = M.lookup to   (tripletSinks :: M.Map Format (Environment -> ConduitT (Position, Position, Position) Void IO ()))
    faceSink      = M.lookup to   (faceSinks    :: M.Map Format (Environment -> ConduitT () Position IO () -> ConduitT () Face IO () -> IO ()))
    --- TODO likely there's more conversions
    result = F.asum -- keep ordered for best experience
      [ pf2Pf    env <$> pfSource      <*> faceSink
      , pf2Tri   env <$> pfSource      <*> tripletSink
      , direct   env <$> tripletSource <*> tripletSink
      , trip2Pos env <$> tripletSource <*> xyzSink
      , trip2Pos env <$> tripletSource <*> xySink
      , pos2Pos  env <$> posSource     <*> xyzSink
      , pos2Pos  env <$> posSource     <*> xySink
      -- TODO pos2string missing due to transformer issue
      ]

    pos2Pos  env fsource fsink = runConduit $ fsource env .| fsink env
    trip2Pos env fsource fsink = runConduit $ fsource env .| untriple .| fsink env
    pf2Tri   env fsource fsink = (\(cv, cf) -> runConduit $ triplet cv cf .| fsink env) $ fsource env
    pf2Pf    env fsource fsink = (\(cv, cf) -> fsink env cv cf) $ fsource env
    direct   env fsource fsink = runConduit $ fsource env .| fsink env

combinations :: Environment -> [(Format, Format)] -- TODO could be implemented to not require Environment
combinations env = filter (\(f, t) -> M.isJust $ resolve env f t) all
  where
    all = (,) <$> formats <*> formats

showCombinations :: [(Format, Format)] -> String
showCombinations = intercalate "\n" . fmap (\(f, t) -> show f ++ " -> " ++ show t)

--- TODO rename
readEnvironment :: Handle -> String -> T.Text -> T.Text -> IO Environment
readEnvironment h p delimVal delimLine = do
  ba1 <- LIO.readFile p
  ba2 <- LIO.readFile p
  bb1 <- BL.readFile p
  bb2 <- BL.readFile p
  pure Environment{ eBlobA1 = ba1, eBlobA2 = ba2, eBlobB1 = bb1, eBlobB2 = bb2, eXyzVal = delimVal, eXyzLine = delimLine, eHandle = h }

main :: IO ()
main = do
  rargs <- execParser opts
  case createArgs rargs of
    Left e         -> die e
    Right Args{..} -> withFile pOut WriteMode (\h -> do
        env <- readEnvironment h pIn ";" "\n"
        when list $ putStrLn $ showCombinations $ combinations env
        run env fIn fOut)
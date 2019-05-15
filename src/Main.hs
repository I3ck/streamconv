{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Conduit
import Types
import Instances ()
import Sources
--import Transformers
import Sinks
import System.IO
import Options.Applicative
import Data.List
import System.Exit
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
  pure Args{pIn = rpIn, pOut = rpOut, fIn = fIn, fOut = fOut}

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
  where
    sFormats = " [" ++ intercalate ", " (show <$> formats) ++ " ]"

--------------------------------------------------------------------------------

--- TODO consider passing Args here
--- TODO expand to support all possible combinations (for now, later consider better abstraction to not have quadratic complexity)
run :: Environment -> Format -> Format -> IO ()
run env = run'
  where
    run' :: Format -> Format -> IO ()
    run' from to = do
      let pfSource      = M.lookup from pfSources
          tripletSource = M.lookup from tripletSources
          posSource     = M.lookup from posSources
          xyzSink       = M.lookup to   xyzSinks
          tripletSink   = M.lookup to   (tripletSinks :: M.Map Format (Environment -> ConduitT (Position, Position, Position) Void IO ()))
          faceSink      = M.lookup to   (faceSinks    :: M.Map Format (Environment -> ConduitT () Position IO () -> ConduitT () Face IO () -> IO ()))
          
          --- TODO likely there's more conversions
          conversion = F.asum -- keep ordered for best experience
            [ pf2Pf   env <$> pfSource      <*> faceSink
            , pf2Tri  env <$> pfSource      <*> tripletSink
            , direct  env <$> tripletSource <*> tripletSink
            , pos2Pos env <$> posSource     <*> xyzSink
            -- TODO pos2string missing due to transformer issue
            ]

      case conversion of
        Nothing -> putStrLn $ "Conversion from " ++ show from ++ " to " ++ show to ++ " not supported (yet)" ---TODO more info
        Just x  -> x

    --pos2Str env fsource fsink = runConduit $ fsource env .| objToStr bufferSize .| fsink env
    pos2Pos env fsource fsink = runConduit $ fsource env .| fsink env
    pf2Tri  env fsource fsink = (\(cv, cf) -> runConduit $ triplet cv cf .| fsink env) $ fsource env
    pf2Pf   env fsource fsink = (\(cv, cf) -> fsink env cv cf) $ fsource env
    direct  env fsource fsink = runConduit $ fsource env .| fsink env

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
        run env fIn fOut)
{-
{-
  runConduit $ 
       (yieldMany $ fmap (\i -> Position i (2*i) (3*i)) [0..10000000-1]) 
    .| xyzToStr bufferSize " " "\n" 
    .| stringSink "tmp/output1.xyz"
{-
  runConduit $ 
       (yieldMany . replicate 1000 $ Position 3 4 5) 
    .| xyToStr bufferSize ";" "\n" 
    .| stringSink "tmp/output2.xy"
  
  runConduit $ 
       xyz "tmp/output1.xyz" " " "\n" 
    .| xyToStr bufferSize  ";" "\n" 
    .| stringSink "tmp/output3.xyz"

  runConduit $ 
       xyz "tmp/output1.xyz" " " "\n" 
    .| objToStr bufferSize 
    .| stringSink "tmp/output.obj"

  runConduit $
       xyz "tmp/output1.xyz" " " "\n"
    .| plyAsciiSink "tmp/output.ply"
-}
  runConduit $
       xyz "tmp/output1.xyz" " " "\n"
    .| plyBinarySink "tmp/outputBin.ply"
-}
  withFile "tmp/outputstlstl.obj" WriteMode (\h -> do
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob
      .| untriple
      .| objToStr bufferSize
      .| stringSink h)

  withFile "tmp/outputstl.ply" WriteMode (\h -> do
    blob <- (LIO.readFile "tmp/stlascii.stl") 
    runConduit $
         stl blob
      .| plyTripletAsciiSink h)

  withFile "tmp/outputstlBIN.ply" WriteMode (\h -> do
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob
      .| plyTripletBinarySink h)

  withFile "tmp/outputstlSTLASCII.stl" WriteMode (\h -> do 
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob
      .| stlAsciiSink h)

  withFile "tmp/outputstlSTLbinary.stl" WriteMode (\h -> do
    blob <- LIO.readFile "tmp/stlascii.stl"
    runConduit $
         stl blob     
      .| stlBinarySink h)

  -- CURRENTLY FAILS, SEE TODO
  withFile "tmp/outputstlAgain.ply" WriteMode (\h -> do
    (cv, cf) <- ply "tmp/outputstl.ply"
    plyAsciiSink' h cv cf
    )

  withFile "tmp/objFile.ply" WriteMode (\h -> do
    (cv, cf) <- obj "tmp/obj.obj"
    plyAsciiSink' h cv cf
    )

  withFile "tmp/objFileToBin.ply" WriteMode (\h -> do
    (cv, cf) <- obj "tmp/obj.obj"
    plyBinarySink' h cv cf
    )
    
  pure ()
-}
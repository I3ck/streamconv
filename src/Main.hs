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
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Maybe as M
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
run :: Environment -> String -> Format -> Format -> IO ()
run sd pt = run'
  where
    run' :: Format -> Format -> IO ()

    run' PlyAscii StlAscii
      = withFile pt WriteMode (\h -> do
        let (cv, cf) = ply sd
        runConduit $ triplet cv cf .| stlAsciiSink h)

    run' PlyAscii StlBinary
      = withFile pt WriteMode (\h -> do
        let (cv, cf) = ply sd
        runConduit $ triplet cv cf .| stlBinarySink h)

    run' PlyAscii Obj
      = withFile pt WriteMode (\h -> do
        let (cv, cf) = ply sd
        objSink h cv cf)

    run' StlAscii Obj
      = withHandle (\h -> runConduit $ stlAscii sd .| objTripletSink h)

    run' StlAscii StlBinary
      = withHandle (\h -> runConduit $ stlAscii sd .| stlBinarySink h)

    run' StlAscii PlyAscii 
      = withHandle (\h -> runConduit $ stlAscii sd .| plyTripletAsciiSink h)

    run' StlAscii PlyBinary
      = withHandle (\h -> runConduit $ stlAscii sd .| plyTripletBinarySink h)

    run' StlBinary Obj
      = withHandle (\h -> runConduit $ stlBinary sd .| objTripletSink h)

    run' StlBinary StlAscii
      = withHandle (\h -> runConduit $ stlBinary sd .| stlAsciiSink h)

    run' StlBinary PlyAscii 
      = withHandle (\h -> runConduit $ stlBinary sd .| plyTripletAsciiSink h)

    run' StlBinary PlyBinary
      = withHandle (\h -> runConduit $ stlBinary sd .| plyTripletBinarySink h)

    run' Obj PlyAscii
      = withFile pt WriteMode (\h -> do
        let (cv, cf) = obj sd
        plyAsciiSink' h cv cf)

    run' Obj PlyBinary
      = withFile pt WriteMode (\h -> do
        let (cv, cf) = obj sd
        plyBinarySink' h cv cf)

    run' Obj StlAscii
      = withFile pt WriteMode (\h -> do
        let (cv, cf) = obj sd
        runConduit $ triplet cv cf .| stlAsciiSink h)

    run' Obj StlBinary
      = withFile pt WriteMode (\h -> do
        let (cv, cf) = obj sd
        runConduit $ triplet cv cf .| stlBinarySink h)

    run' Xyz Obj
      = withHandle (\h -> runConduit $ xyz sd .| objToStr bufferSize .| stringSink h)

    run' Xyz PlyAscii 
      = withHandle (\h -> runConduit $ xyz sd .| plyAsciiSink h)

    run' Xyz PlyBinary
      = withHandle (\h -> runConduit $ xyz sd .| plyBinarySink h)

{- TODO implement
    run' Obj PlyBin
      = withFile pt WriteMode (\h -> do
        (cv, cf) <- obj pf
        plyBinSink' h cv cf)
-}

    
    run' f t = putStrLn $ "Conversion from " ++ show f ++ " to " ++ show t ++ " not supported (yet)" ---TODO more info

    withHandle :: (Handle -> IO ()) -> IO ()
    withHandle f = withFile pt WriteMode (\h -> f h)

readEnvironment :: String -> T.Text -> T.Text -> IO Environment
readEnvironment p delimVal delimLine = do
  ba1 <- LIO.readFile p
  ba2 <- LIO.readFile p
  bb1 <- BL.readFile p
  bb2 <- BL.readFile p
  pure Environment{ eBlobA1 = ba1, eBlobA2 = ba2, eBlobB1 = bb1, eBlobB2 = bb2, eXyzVal = delimVal, eXyzLine = delimLine }

main :: IO ()
main = do
  rargs <- execParser opts
  case createArgs rargs of
    Left e         -> die e
    Right Args{..} -> do 
      sd <- readEnvironment pIn ";" "\n"
      run sd pOut fIn fOut
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
module Sinks
  ( stringSink 
  , plyAsciiSink
  ) where

import Classes
import Data.Void
import Conduit
import System.IO

--------------------------------------------------------------------------------

stringSink :: String -> ConduitT String Void IO ()
stringSink path = do
  h <- liftIO $ openFile path WriteMode --TODO consider withFile
  go h
  liftIO $ hClose h
  where
    go h = do
      may <- await
      case may of
        Nothing -> pure ()
        Just x -> do
          liftIO $ hPutStr h x
          go h

--------------------------------------------------------------------------------

plyAsciiSink :: (X a, Y a, Z a) => String -> ConduitT a Void IO ()
plyAsciiSink path = do
  h <- liftIO $ openFile path WriteMode --TODO consider withFile
  liftIO $ hPutStrLn h "ply"
  liftIO $ hPutStrLn h "format ascii 1.0"
  placeholderPos <- liftIO $ hTell h
  liftIO $ hPutStrLn h placeholder
  liftIO $ hPutStrLn h "property float x"
  liftIO $ hPutStrLn h "property float y"
  liftIO $ hPutStrLn h "property float z"
  liftIO $ hPutStrLn h "end_header"
  go h 0 placeholderPos
  liftIO $ hClose h
  where
    go h count placeholderPos = do
      may <- await
      case may of
        Just x -> do
          liftIO $ hPutStr h (toStr x)
          go h (count+1) placeholderPos
        Nothing -> do
          let placeholderlength = length placeholder
              replacement = "element vertex " ++ show count ++ "\ncomment "
          liftIO $ hSeek h AbsoluteSeek placeholderPos
          liftIO $ hPutStrLn h $ replacement ++ replicate (placeholderlength - length replacement) '#'


    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"

    placeholder = "element vertex 0\ncomment ##################################"

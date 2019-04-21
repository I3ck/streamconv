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
  liftIO $ hPutStrLn h "element vertex 0" ---TODO must update this count later on
  liftIO $ hPutStrLn h "property float x"
  liftIO $ hPutStrLn h "property float y"
  liftIO $ hPutStrLn h "property float z"
  liftIO $ hPutStrLn h "end_header"
  go h 0
  liftIO $ hClose h
  where
    go h count = do
      may <- await
      case may of
        Nothing -> pure () --TODO update count here "element vertex (show count)"
        Just x -> do
          liftIO $ hPutStr h (toStr x)
          go h (count+1)

    --- TODO have this helper rather in transformers? (Could already reuse a generalized version)
    toStr v = (show . getx $ v) ++ " " ++ (show . gety $ v) ++ " " ++ (show . getz $ v) ++ " \n" --TODO use "showS trick"

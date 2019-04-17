{-# LANGUAGE RecordWildCards #-}

module Main where

import Conduit
import Types
import Classes
import Instances ()
import System.IO
--import Control.Monad
--import qualified Data.ByteString as BS
import qualified Data.Text as T

--------------------------------------------------------------------------------

main :: IO ()
main = do --withSourceFile "input.tmp" $ \source ->
       --withSinkFile "output.tmp" $ \sink -> do
       --runConduit $ source .| decodeUtf8C .| test .| encodeUtf8C .| sink
       runConduit $ fileChars "input.tmp" .| test2 .| sinkList
       runConduit $ (yieldMany . repeat $ Position 3 4 5) .| xyzSink ";" "\n" "output.tmp"
       pure ()

--------------------------------------------------------------------------------

-- TODO parser that emits chunks of e.g. 3 vertices + normal etc.

test :: ConduitT T.Text T.Text IO ()
test = do 
    Just x <- await --TODO could fail
    lift $ putStrLn $ show x
    yield x

test2 :: ConduitT Char Char IO ()
test2 = go
  where
    go = do 
      may <- await --TODO could fail
      case may of
        Nothing -> pure ()
        Just x  -> do 
            lift $ putStrLn [x]
            yield x
            go

fileChars :: String -> ConduitT () Char IO ()
fileChars path = do
    str <- liftIO $ readFile path
    yieldMany str

--- TODO strong types for path, delimVal delimLine
xyzSink :: (X a, Y a, Z a) => String -> String -> String -> ConduitT a Void IO ()
xyzSink delimval delimline path = do
  h <- liftIO $ openFile path WriteMode --TODO consider withFile
  go h
  liftIO $ hClose h
  where
    go h = do
      may <- await
      case may of
        Nothing -> pure ()
        Just x  -> do 
          liftIO $ hPutStr h (toStr x)
          go h
    toStr v =  (show . getx $ v) 
            ++ delimval 
            ++ (show . gety $ v) 
            ++ delimval 
            ++ (show . getz $ v)
            ++ delimline
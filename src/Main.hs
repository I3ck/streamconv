{-# LANGUAGE RecordWildCards #-}

module Main where

import Conduit
import Types
import Classes
import Instances ()
import Producers
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
       runConduit $ (yieldMany . replicate 1000 $ Position 3 4 5) .| mapC (xyzToStr ";" "\n") .| stringSink "output1.tmp"
       runConduit $ (yieldMany . replicate 1000 $ Position 3 4 5) .| mapC (xyToStr  ";" "\n") .| stringSink "output2.tmp"
       runConduit $ xyz "output1.tmp" ";" "\n" .| mapC (xyToStr  ";" "\n") .| stringSink "output3.tmp"
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

xyToStr :: (X a, Y a) => String -> String -> a -> String
xyToStr delimval delimline v =
     (show . getx $ v) 
  ++ delimval 
  ++ (show . gety $ v) 
  ++ delimline

xyzToStr :: (X a, Y a, Z a) => String -> String -> a -> String
xyzToStr delimval delimline v =
     (show . getx $ v) 
  ++ delimval 
  ++ (show . gety $ v) 
  ++ delimval 
  ++ (show . getz $ v)
  ++ delimline

---TODO rename since writes to file
---TODO consider usage Text everywhere
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
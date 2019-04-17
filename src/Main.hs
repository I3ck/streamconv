{-# LANGUAGE RecordWildCards #-}

module Main where

import Conduit
--import Control.Monad
--import qualified Data.ByteString as BS
import qualified Data.Text as T

--------------------------------------------------------------------------------

main :: IO ()
main = do --withSourceFile "input.tmp" $ \source ->
       --withSinkFile "output.tmp" $ \sink -> do
       --runConduit $ source .| decodeUtf8C .| test .| encodeUtf8C .| sink
       runConduit $ fileChars "input.tmp" .| test2 .| sinkList
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
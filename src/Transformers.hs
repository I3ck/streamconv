module Transformers
  ( xyToStr --- TODO drop or move to sinks
  , untriple
  ) where

import Classes
import Conduit

---TODO names are weird, think of something else

--------------------------------------------------------------------------------

untriple :: (Monad m) => ConduitT (a, a, a) a m ()
untriple = go
  where 
    go = do
      may <- await
      case may of
        Nothing        -> pure ()
        Just (x, y, z) -> do
          yield x
          yield y
          yield z
          go

--------------------------------------------------------------------------------

xyToStr :: (Monad m, X a, Y a) => Int -> String -> String -> ConduitT a String m ()
xyToStr bufferSize delimval delimline = bufferedToStr bufferSize (xyToStr' delimval delimline)

--------------------------------------------------------------------------------

bufferedToStr :: (Monad m) => Int -> (a -> String) -> ConduitT a String m ()
bufferedToStr bufferSize f = go []
  where
    go buffer = do
      may <- await
      case may of
        Nothing -> yield $ concatMap f buffer
        Just v  -> if length buffer > bufferSize
                   then do 
                     yield $ concatMap f (v : buffer)
                     go []
                   else go (v : buffer)

--------------------------------------------------------------------------------

xyToStr' :: (X a, Y a) => String -> String -> a -> String
xyToStr' delimval delimline v =
    (((show . getx $ v) ++)
  . (delimval ++)
  . ((show . gety $ v) ++)
  . (delimline ++))
  ""

--------------------------------------------------------------------------------
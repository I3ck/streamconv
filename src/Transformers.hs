module Transformers
  ( xyToStr
  , xyzToStr    
  , objToStr
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

objToStr :: (Monad m, X a, Y a, Z a) => Int -> ConduitT a String m ()
objToStr bufferSize = bufferedToStr bufferSize objToStr'

--------------------------------------------------------------------------------

xyToStr :: (Monad m, X a, Y a) => Int -> String -> String -> ConduitT a String m ()
xyToStr bufferSize delimval delimline = bufferedToStr bufferSize (xyToStr' delimval delimline)

--------------------------------------------------------------------------------

xyzToStr :: (Monad m, X a, Y a, Z a) => Int -> String -> String -> ConduitT a String m ()
xyzToStr bufferSize delimval delimline = bufferedToStr bufferSize (xyzToStr' delimval delimline)

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

objToStr' :: (X a, Y a, Z a) => a -> String
objToStr' v =
    (("v " ++)
  . ((show . getx $ v) ++)
  . (' ' :)
  . ((show . gety $ v) ++)
  . (' ' :)
  . ((show . getz $ v) ++)
  . ('\n' :))
  ""

--------------------------------------------------------------------------------

xyToStr' :: (X a, Y a) => String -> String -> a -> String
xyToStr' delimval delimline v =
    (((show . getx $ v) ++)
  . (delimval ++)
  . ((show . gety $ v) ++)
  . (delimline ++))
  ""

--------------------------------------------------------------------------------

xyzToStr' :: (X a, Y a, Z a) => String -> String -> a -> String
xyzToStr' delimval delimline v =
    (((show . getx $ v) ++)
  . (delimval ++)
  . ((show . gety $ v) ++)
  . (delimval ++)
  . ((show . getz $ v) ++)
  . (delimline ++))
  ""

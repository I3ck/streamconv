module Transformers
  ( xyToStrC
  , xyzToStrC    
  ) where

import Classes
import Conduit

--------------------------------------------------------------------------------

xyToStrC :: (Monad m, X a, Y a) => String -> String -> ConduitT a String m ()
xyToStrC delimval delimline = go []
  where
    bufferSize = 100 --TODO param
    go buffer = do
      may <- await
      case may of
        Nothing -> yield $ concatMap (xyToStr delimval delimline) buffer
        Just v  -> if length buffer > bufferSize
                   then do 
                     yield $ concatMap (xyToStr delimval delimline) (v : buffer)
                     go []
                   else go (v : buffer)

--------------------------------------------------------------------------------

xyzToStrC :: (Monad m, X a, Y a, Z a) => String -> String -> ConduitT a String m ()
xyzToStrC delimval delimline = go []
  where
    bufferSize = 100 --TODO param
    go buffer = do
      may <- await
      case may of
        Nothing -> yield $ concatMap (xyzToStr delimval delimline) buffer
        Just v  -> if length buffer > bufferSize
                   then do 
                     yield $ concatMap (xyzToStr delimval delimline) (v : buffer)
                     go []
                   else go (v : buffer)

--------------------------------------------------------------------------------

xyToStr :: (X a, Y a) => String -> String -> a -> String
xyToStr delimval delimline v =
    (((show . getx $ v) ++)
  . (delimval ++)
  . ((show . gety $ v) ++)
  . (delimline ++))
  ""

--------------------------------------------------------------------------------

xyzToStr :: (X a, Y a, Z a) => String -> String -> a -> String
xyzToStr delimval delimline v =
    (((show . getx $ v) ++)
  . (delimval ++)
  . ((show . gety $ v) ++)
  . (delimval ++)
  . ((show . getz $ v) ++)
  . (delimline ++))
  ""

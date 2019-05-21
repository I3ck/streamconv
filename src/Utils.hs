module Utils 
  ( faceNormal
  , toPos --- TODO consider refactoring away
  , maybeRead
  , maybeToEither
  ) where

import Types
import Classes

import qualified Data.Maybe as M

--------------------------------------------------------------------------------

toPos :: (X a, Y a, Z a) => a -> Position
toPos v = Position (getx v) (gety v) (getz v)

--------------------------------------------------------------------------------

faceNormal :: Position -> Position -> Position -> Normal
faceNormal a b c = n
  where
    n = norm $ cross u v
    u = dir a b
    v = dir a c

--------------------------------------------------------------------------------

dir :: Position -> Position -> Position
dir (Position x1 y1 z1) (Position x2 y2 z2) = Position (x2-x1) (y2-y1) (z2-z1)

--------------------------------------------------------------------------------

cross :: Position -> Position -> Position
cross (Position a1 a2 a3) (Position b1 b2 b3) = Position (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)

--------------------------------------------------------------------------------

norm :: Position -> Normal -- TODO could fail
norm p@(Position x y z) = Normal (x/l) (y/l) (z/l)
  where
    l = Utils.abs p

--------------------------------------------------------------------------------

abs :: Position -> Double
abs (Position x y z) = sqrt $ x*x + y*y + z*z

--------------------------------------------------------------------------------

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = valueIfNoRemainder =<< (M.listToMaybe . reads $ s)
  where
    valueIfNoRemainder (x, rem) | null rem  = Just x
                                | otherwise = Nothing

--------------------------------------------------------------------------------

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _   (Just x) = Right x
maybeToEither err Nothing  = Left err
                                

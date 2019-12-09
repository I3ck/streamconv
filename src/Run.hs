module Run
  ( showCombinations
  , combinations
  , run
  ) where

import           Instances     ()
import           Sinks
import           Sources
import           Transformers
import           Types

import           Conduit
import qualified Data.Foldable as F
import           Data.List
import qualified Data.Map      as M
import qualified Data.Maybe    as M

--------------------------------------------------------------------------------

--- TODO consider passing Args here
--- TODO expand to support all possible combinations (for now, later consider better abstraction to not have quadratic complexity)
run :: Environment -> Format -> Format -> IO ()
run env from to = case resolve env from to of
  Nothing -> putStrLn $ "Conversion from " ++ show from ++ " to " ++ show to ++ " not supported (yet)" ---TODO more info
  Just x  -> x

--------------------------------------------------------------------------------

resolve :: Environment -> Format -> Format -> Maybe (IO ())
resolve env from to = result
  where
    pfSource      = M.lookup from pfSources
    tripletSource = M.lookup from tripletSources
    posSource     = M.lookup from posSources
    xyzSink       = M.lookup to   xyzSinks
    xySink        = M.lookup to   xySinks
    tripletSink   = M.lookup to   (tripletSinks :: M.Map Format (Environment -> ConduitT (Position, Position, Position) Void IO ()))
    faceSink      = M.lookup to   (faceSinks    :: M.Map Format (Environment -> ConduitT () Position IO () -> ConduitT () Face IO () -> IO ()))
    --- TODO likely there's more conversions
    result = F.asum -- keep ordered for best experience
      [ pf2Pf    env <$> pfSource      <*> faceSink
      , pf2Tri   env <$> pfSource      <*> tripletSink
      , direct   env <$> tripletSource <*> tripletSink
      , trip2Pos env <$> tripletSource <*> xyzSink
      , trip2Pos env <$> tripletSource <*> xySink
      , pos2Pos  env <$> posSource     <*> xyzSink
      , pos2Pos  env <$> posSource     <*> xySink
      -- TODO pos2string missing due to transformer issue
      ]

    pos2Pos  env fsource fsink = runConduit $ fsource env .| fsink env
    trip2Pos env fsource fsink = runConduit $ fsource env .| untriple .| fsink env
    pf2Tri   env fsource fsink = (\(cv, cf) -> runConduit $ triple env cv cf .| fsink env) $ fsource env
    pf2Pf    env fsource fsink = (\(cv, cf) -> fsink env cv cf) $ fsource env
    direct   env fsource fsink = runConduit $ fsource env .| fsink env

--------------------------------------------------------------------------------

combinations :: Environment -> [(Format, Format)] -- TODO could be implemented to not require Environment
combinations env = filter (\(f, t) -> M.isJust $ resolve env f t) all
  where
    all = (,) <$> formats <*> formats

--------------------------------------------------------------------------------

showCombinations :: [(Format, Format)] -> String
showCombinations = intercalate "\n" . fmap (\(f, t) -> show f ++ " -> " ++ show t)

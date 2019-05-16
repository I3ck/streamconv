module Transformers
  ( untriple
  ) where

import Conduit

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
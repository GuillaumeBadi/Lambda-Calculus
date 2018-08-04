
module Mutation(mutate) where

import Types
import Commons
import Generator

mutate :: Int -> Int -> [LE] -> LE -> IO LE

mutate _ _ vs v@Va{} = gen vs >>= return

mutate md cd vs (Ap e e') = do
  let p = trslt 0.0 1.0 0 (fromIntegral md) (fromIntegral cd)
  go <- coin p
  if go
     then do
        b <- pk [0, 1]
        case b of
          0 -> mutate md (cd + 1) vs e >>= \ne -> return $ Ap ne e'
          1 -> mutate md (cd + 1) vs e >>= \ne' -> return $ Ap e ne'
        
     else gen vs >>= return

mutate md cd vs (Ab v e) = do
  let p = trslt 0.0 1.0 0 (fromIntegral md) (fromIntegral cd)
  go <- coin p
  if go
     then mutate md (cd + 1) (v:vs) e >>= (return . Ab v)
     else gen (v:vs) >>= return

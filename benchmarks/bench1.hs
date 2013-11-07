{-# LANGUAGE CPP #-}
module Main
       ( main -- :: IO ()
       ) where

import Criterion.Main
import Crypto.Hash.BLAKE

import Control.DeepSeq
import qualified Data.ByteString as B

--------------------------------------------------------------------------------

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bench "blake256" $ nf blake256 (B.pack [1..512])
  , bench "blake512" $ nf blake512 (B.pack [1..512])
  ]

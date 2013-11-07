{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Crypto.Hash.BLAKE
-- Copyright   : (c) Austin Seipp 2013
-- License     : MIT
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : portable
--
-- BLAKE-256 and BLAKE-512 hashes. The underlying implementation uses
-- the @ref@ code of @blake256@ and @blake512@ from SUPERCOP, and
-- should be relatively fast.
--
-- For more information visit <https://131002.net/blake/>.
--
module Crypto.Hash.BLAKE
       ( blake256 -- :: ByteString -> ByteString
       , blake512 -- :: ByteString -> ByteString
       ) where

import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           System.IO.Unsafe         (unsafePerformIO)

import           Data.ByteString          (ByteString)
import           Data.ByteString.Internal (create)
import           Data.ByteString.Unsafe   (unsafeUseAsCStringLen)

-- | Compute a 256-bit (32 byte) digest of an input string.
blake256 :: ByteString -> ByteString
blake256 xs =
  -- SHA256 has 32 bytes of output
  unsafePerformIO . create 32 $ \out ->
    unsafeUseAsCStringLen xs $ \(cstr,clen) ->
      c_blake256 out cstr (fromIntegral clen) >> return ()
{-# INLINE blake256 #-}

-- | Compute a 512-bit (64 byte) digest of an input string.
blake512 :: ByteString -> ByteString
blake512 xs =
  -- The default primitive of SHA512 has 64 bytes of output.
  unsafePerformIO . create 64 $ \out ->
    unsafeUseAsCStringLen xs $ \(cstr,clen) ->
      c_blake512 out cstr (fromIntegral clen) >> return ()
{-# INLINE blake512 #-}

--
-- FFI hash binding
--

foreign import ccall unsafe "blake256"
  c_blake256 ::Ptr Word8 -> Ptr CChar -> CULLong -> IO CInt

foreign import ccall unsafe "blake512"
  c_blake512 ::Ptr Word8 -> Ptr CChar -> CULLong -> IO CInt

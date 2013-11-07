{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Main
       ( main  -- :: IO ()
       ) where
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S

import           Crypto.Hash.BLAKE

import           System.Environment       (getArgs)
import           Test.QuickCheck
import           Test.QuickCheck.Property (morallyDubiousIOProperty)
import           Text.Printf

--------------------------------------------------------------------------------
-- Orphans

instance Arbitrary ByteString where
  arbitrary = S.pack `liftM` arbitrary

--------------------------------------------------------------------------------
-- Signatures

pure256 :: ByteString -> Bool
pure256 xs = blake256 xs == blake256 xs

pure512 :: ByteString -> Bool
pure512 xs = blake512 xs == blake512 xs

length256 :: ByteString -> Bool
length256 xs = S.length (blake256 xs) == 32

length512 :: ByteString -> Bool
length512 xs = S.length (blake512 xs) == 64

--------------------------------------------------------------------------------
-- Driver

main :: IO ()
main = do
  args <- fmap (drop 1) getArgs
  let n = if null args then 100 else read (head args) :: Int
  (results, passed) <- runTests n
  printf "Passed %d tests!\n" (sum passed)
  unless (and results) (fail "Not all tests passed!")

runTests :: Int -> IO ([Bool], [Int])
runTests ntests = fmap unzip . forM (tests ntests) $ \(s, a) ->
  printf "%-40s: " s >> a

tests :: Int -> [(String, IO (Bool,Int))]
tests ntests =
  [ ("blake256 purity", wrap pure256)
  , ("blake256 length", wrap length256)
  , ("blake512 purity", wrap pure512)
  , ("blake512 length", wrap length512)
  ]
  where
    wrap :: Testable prop => prop -> IO (Bool, Int)
    wrap prop = do
      r <- quickCheckWithResult stdArgs{maxSize=ntests} prop
      case r of
        Success n _ _           -> return (True, n)
        GaveUp  n _ _           -> return (True, n)
#if MIN_VERSION_QuickCheck(2,6,0)
        Failure n _ _ _ _ _ _ _ -> return (False, n)
#else
        Failure n _ _ _ _ _ _   -> return (False, n)
#endif
        _                       -> return (False, 0)

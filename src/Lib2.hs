module Lib2
       (
       ) where

import Data.Monoid ((<>))
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Ratio ((%))
import qualified Data.Ratio as R

bounded1 :: IO ()
bounded1 = do
  putStrLn $ (show (minBound :: Int)) <> " " <> (show (maxBound :: Int)) 
  putStrLn $ (show (minBound :: Int64)) <> " " <> (show (maxBound :: Int64)) 
  putStrLn $ (show (minBound :: Int32)) <> " " <> (show (maxBound :: Int32)) 
  putStrLn $ (show (minBound :: Int16)) <> " " <> (show (maxBound :: Int16)) 
  putStrLn $ (show (minBound :: Int8)) <> " " <> (show (maxBound :: Int8)) 

  putStrLn $ (show (minBound :: Word)) <> " " <> (show (maxBound :: Word)) 
  putStrLn $ (show (minBound :: Word64)) <> " " <> (show (maxBound :: Word64)) 
  putStrLn $ (show (minBound :: Word32)) <> " " <> (show (maxBound :: Word32)) 
  putStrLn $ (show (minBound :: Word16)) <> " " <> (show (maxBound :: Word16)) 
  putStrLn $ (show (minBound :: Word8)) <> " " <> (show (maxBound :: Word8)) 



num1 :: IO ()
num1 = do
  putStrLn $ show $ f (100::Int16)
  putStrLn $ show $ f (100::Word16)
  putStrLn $ show $ f (-100::Int16)
  putStrLn $ show $ f (-100::Word16)
  putStrLn $ show $ f (0.123::Float)
  putStrLn $ show $ f (0.123::Double)
  putStrLn $ show $ f (-0.123::Float)
  putStrLn $ show $ f (-0.123::Double)
  where
    f :: Num a => a -> a
    f x = (negate x) * 2 + (abs x) + (signum x) - x + 3 


num2 :: IO ()
num2 = do
  putStrLn $ show $ (fromInteger :: Integer -> Int8) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Int16) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Int32) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Int64) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Int) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Int16) (-1000)
  
  putStrLn $ show $ (fromInteger :: Integer -> Word8) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Word16) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Word32) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Word64) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Word) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Word16) (-1000)
  
  putStrLn $ show $ (fromInteger :: Integer -> Float) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Float) (-1000)
  putStrLn $ show $ (fromInteger :: Integer -> Double) 1000
  putStrLn $ show $ (fromInteger :: Integer -> Double) (-1000)
  
  putStrLn $ show $ (fromInteger :: Integer -> Rational) (1000)
  

-- 分数
rational1 :: IO ()
rational1 = do
  putStrLn $ show $ (2 % 3)
  putStrLn $ show $ (4 % 6)
  putStrLn $ show $ (4 % 6) * (1 % 6)


-- 分数 -> 整数
rational2 :: IO ()
rational2 =
  let
    r = 4 % 6
  in putStrLn $ (show $ R.numerator r) <> " " <> (show $ R.denominator r)


-- (整数,少数) -> 分数
real1 :: IO ()
real1 = do
  putStrLn $ show $ toRational (100 :: Int32)
  putStrLn $ show $ toRational (100 :: Word32)
  putStrLn $ show $ toRational (0.123 :: Float)
  putStrLn $ show $ toRational (0.123 :: Double)


-- 商(整数)
integral1 :: IO ()
integral1 = do
  putStrLn $ (show $ (7::Int32) `div` 3) <> " " <> (show $ 7 `mod` 3)
  putStrLn $ (show $ (7::Int32) `quot` 3) <> " " <> (show $ 7 `rem` 3)
  putStrLn $ (\(d,m)-> show d <> " " <> show m ) $ divMod (7::Int32) 3
  putStrLn $ (\(q,r)-> show q <> " " <> show r ) $ quotRem (7::Int32) 3
  putStrLn $ show $ toInteger (100 :: Int32)
  putStrLn $ show $ toInteger (100 :: Word32)


-- 除(少数)
fractional1 :: IO ()
fractional1 = do
  putStrLn $ show $ (0.123 :: Float) / 1.111
  putStrLn $ show $ (0.123 :: Double) / 1.111
  putStrLn $ show $ recip (0.123 :: Float)
  putStrLn $ show $ recip (0.123 :: Double)


-- 分数 -> 少数
fractional2 :: IO ()
fractional2 = do
  putStrLn $ show $ fromRational $ 1 % 2
  putStrLn $ show $ fromRational $ 4 % 7
  putStrLn $ show $ fromRational $ 1 % 1000


-- 少数 -> 整数 切り上げ,切り捨て,四捨五入
realfrac1 = do
  putStrLn $ show $ properFraction 1.1
  putStrLn $ show $ truncate 1.1
  putStrLn $ show $ round 1.4
  putStrLn $ show $ round 1.6
  putStrLn $ show $ ceiling 1.4
  putStrLn $ show $ ceiling 1.6
  putStrLn $ show $ floor 1.4
  putStrLn $ show $ floor 1.6

  putStrLn $ show $ properFraction (-1.1)
  putStrLn $ show $ truncate (-1.1)
  putStrLn $ show $ round (-1.4)
  putStrLn $ show $ round (-1.6)
  putStrLn $ show $ ceiling (-1.4)
  putStrLn $ show $ ceiling (-1.6)
  putStrLn $ show $ floor (-1.4)
  putStrLn $ show $ floor (-1.6)


-- 整数 -> (整数,少数,分数)
fromIntegral1 :: IO ()
fromIntegral1 = do
  putStrLn $ show $ (fromIntegral :: Int16  -> Word16) $ -100
  putStrLn $ show $ (fromIntegral :: Word16 -> Int16)  $ 100
  putStrLn $ show $ (fromIntegral :: Int16  -> Int8)   $ 300
  putStrLn $ show $ (fromIntegral :: Int16  -> Float)  $ -100
  putStrLn $ show $ (fromIntegral :: Word16 -> Double) $ 100
  putStrLn $ show $ (fromIntegral :: Int16  -> Rational) $ -100
  putStrLn $ show $ (fromIntegral :: Word16 -> Rational) $ 100


-- (整数,少数,分数) -> 少数
realToFrac1 :: IO ()
realToFrac1 = do
  putStrLn $ show $ (realToFrac :: Int16 -> Float)  $ 100
  putStrLn $ show $ (realToFrac :: Int16 -> Double) $ 100
  putStrLn $ show $ (realToFrac :: Word16 -> Float)  $ 100
  putStrLn $ show $ (realToFrac :: Word16 -> Double) $ 100
  putStrLn $ show $ (realToFrac :: Float -> Double) $ 0.123
  putStrLn $ show $ (realToFrac :: Double -> Float) $ 0.123
  putStrLn $ show $ (realToFrac :: Rational -> Float)  $ 2 % 3
  putStrLn $ show $ (realToFrac :: Rational -> Double) $ 2 % 3


{-# LANGUAGE OverloadedStrings #-}

module Lib1
    ( someFunc
    ) where

import Data.Monoid ((<>))
import Control.Monad.Writer.Lazy

import Text.Read (Read(..),Lexeme(..),readListPrecDefault,lexP)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Text.Lazy.Builder.Int as LTB
import Data.Text.Lazy.Builder.RealFloat as LTB

import qualified Data.Text.Read as TR
import qualified Data.Text.Lazy.Read as LTR

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import Data.Word (Word8)
import Data.Bits (shift)


someFunc :: IO ()
someFunc = putStrLn "someFunc"


print1 :: IO ()
print1 = putStrLn "Hello"

print2 :: IO ()
print2 = print 101

print3 :: IO ()
print3 = print "Hello"

print4 :: IO ()
print4 = putStrLn $ show "Hello"

print5 :: IO ()
print5 = putStrLn $ show 101



data Some = Some Int Int Int
          deriving (Show,Read)

show1 :: IO ()
show1 = putStrLn $ show (Some 101 102 103)


data Date = Date Int Int Int
instance Show Date where
  show (Date y m d) = (show y) <> "/" <> (show m) <> "/" <> (show d)

show2 :: IO ()
show2 = putStrLn $ show (Date 2018 3 4)



newtype Date' = Date' { getDate :: Date }
instance Show Date' where
  show (Date'(Date y m d)) = (show y) <> "-" <> (show m) <> "-" <> (show d)

show3 :: IO ()
show3 = putStrLn $ show $ Date' (Date 2018 3 4)



read1 :: IO ()
read1 =
  let
    n1 = (read :: String -> Float) "101"
    n2 = (read :: String -> Float) "10.123"
  in putStrLn $ show $ n1 + n2

read2 :: IO ()
read2 = 
  let
    (Some a b c) = (read :: String -> Some) "Some 11 22 33"
  in putStrLn $ (show a) <> " " <> (show b) <> " " <> (show c) 


instance Read Date where
  readPrec = do
    y <- readPrec
    Symbol "/" <- lexP
    m <- readPrec
    Symbol "/" <- lexP
    d <- readPrec
    return $ Date y m d
    
  readListPrec = readListPrecDefault

read3 :: IO ()
read3 = putStrLn $ show $ (read :: String -> Date) "2018/05/06"


instance Read Date' where
  readPrec = do
    y <- readPrec
    Symbol "-" <- lexP
    m <- readPrec
    Symbol "-" <- lexP
    d <- readPrec
    return $ Date' $ Date y m d
    
  readListPrec = readListPrecDefault

read4 :: IO ()
read4 = putStrLn $ show $ getDate $ (read :: String -> Date') "2018-05-06"








text1 :: IO ()
text1 = putStrLn $ T.unpack $ T.pack $ "Hello"

text2 :: IO ()
text2 = T.putStrLn $ T.pack $ "Hello"

text3 :: IO ()
text3 = T.putStrLn "Hello" -- work with {-# LANGUAGE OverloadedStrings #-}

text4 :: IO ()
text4 =
  let
    l1 = (length :: [Char] -> Int) "Hello, World!こんにちは"
    l2 = T.length "Hello, World!こんにちは"
  in putStrLn ( (show l1) <> " " <> (show l2))




textconcat1 :: IO ()
textconcat1 = do
  putStrLn $ "Hello" ++ ", World!"
  putStrLn $ "Hello" <> ", World!"
  
textconcat2 :: IO ()
textconcat2 = do
  T.putStrLn $ "Hello" `T.append` ", World!"
  T.putStrLn $ "Hello" <> ", World!"
  
textconcat3 :: IO ()
textconcat3 = do
  T.putStrLn $ LT.toStrict $ LT.pack "Hello"
  LT.putStrLn $ LT.fromStrict $ T.pack "Hello"
  LT.putStrLn $ "Hello" `LT.append` ", World!"
  LT.putStrLn $ "Hello" <> ", World!"

textconcat4 :: IO ()
textconcat4 = do
  LT.putStrLn $ "Hello" <> ", " <> (LT.pack $ show 101) <> " World! " <> (LT.pack $ show 0.123)
  LT.putStrLn $ LTB.toLazyText $ "Hello, " <> LTB.decimal 101 <> " World! " <> LTB.realFloat 0.123 

textconcat5 :: IO ()
textconcat5 = LT.putStrLn $ LTB.toLazyText $ execWriter f
  where
    f :: Writer LTB.Builder ()
    f = do
      tell "Hello\n"
      tell "Hello\n"
      tell (LTB.decimal 1001) >> tell "\n"
      tell (LTB.hexadecimal 1024) >> tell "\n"
      tell (LTB.realFloat 0.123) >> tell "\n"
      mapM_ (\i -> tell "Hello" >> tell (LTB.decimal i) >> tell "\n") [0..9] 
      tell "World!\n"
      tell "World!\n"
      
textread1 :: IO ()
textread1 =
  let
    Right (n1,_) = TR.decimal "101"
    Right (n2,_) = TR.hexadecimal "FF"
    Right (n3,_) = (TR.signed TR.decimal) "-101"
    Right (n4,_) = (TR.signed TR.hexadecimal) "-FF"
    Right (n5,_) = TR.rational "0.123"
    Right (n6,_) = (TR.signed TR.rational) "-0.123"
    Right (n7,_) = TR.double "0.123"
    Right (n8,_) = (TR.signed TR.double) "-0.123"
  in putStrLn $ show (n1,n2,n3,n4,n5,n6,n7,n8)

textread2 :: IO ()
textread2 =
  let
    Right (n1,_) = LTR.decimal "101"
    Right (n2,_) = LTR.hexadecimal "FF"
    Right (n3,_) = (LTR.signed LTR.decimal) "-101"
    Right (n4,_) = (LTR.signed LTR.hexadecimal) "-FF"
    Right (n5,_) = LTR.rational "0.123"
    Right (n6,_) = (LTR.signed LTR.rational) "-0.123"
    Right (n7,_) = LTR.double "0.123"
    Right (n8,_) = (LTR.signed LTR.double) "-0.123"
  in putStrLn $ show (n1,n2,n3,n4,n5,n6,n7,n8)

textread3 :: IO ()
textread3 = do
  s <- T.getLine
  case TR.decimal s of
    Left e      -> putStrLn $ "error: " <> e
    Right (n,_) -> putStrLn $ "number: " <> show n

textread4 :: IO ()
textread4 = do
  s <- T.getLine
  case (TR.signed TR.decimal) s of
    Left e      -> putStrLn $ "error: " <> e
    Right (n,_) -> putStrLn $ "number: " <> show n




bs1 :: IO ()
bs1 = putStr $ decode $ BS.unpack $ BS.pack [72,101,108,108,111,10] -- "Hello"
  where
    decode = fmap (toEnum . fromIntegral)

bs2 :: IO ()
bs2 = BS.putStr $ BS.pack [72,101,108,108,111,10] -- "Hello"

bs3 :: IO ()
bs3 = do
  BS.putStr "Hello\n" -- work with {-# LANGUAGE OverloadedStrings #-}

bs4 :: IO ()
bs4 =
  let
    l1 = (length :: [Char] -> Int) "Hello, World!こんにちは"
    l2 = T.length "Hello, World!こんにちは"
    l3 = BS.length "Hello, World!KNNCH"
  in putStrLn ( (show l1) <> " " <> (show l2) <> " " <> (show l3))


bstext1 :: IO ()
bstext1 = do
  putStrLn $ show $ length $ BS.unpack $ TE.encodeUtf8 "こんにちは"
  putStrLn $ show $ length $ BS.unpack $ TE.encodeUtf16LE "こんにちは"
  putStrLn $ show $ length $ BS.unpack $ TE.encodeUtf32LE "こんにちは"
  
bstext2 :: IO ()
bstext2 = do
  putStrLn $ show $ length $ BS.unpack $ TE.encodeUtf8 "Hello"
  putStrLn $ show $ length $ BS.unpack $ TE.encodeUtf16LE "Hello"
  putStrLn $ show $ length $ BS.unpack $ TE.encodeUtf32LE "Hello"



bsconcat1 :: IO ()
bsconcat1 = do
  BS.putStr $ "Hello" `BS.append` ", World!\n"
  BS.putStr $ "Hello" <> ", World!\n"
  
bsconcat2 :: IO ()
bsconcat2 = do
  BS.putStr $ LBS.toStrict $ LBS.pack [72,101,108,108,111,10] -- "Hello"
  LBS.putStr $ LBS.fromStrict $ BS.pack [72,101,108,108,111,10] -- "Hello"
  LBS.putStr $ "Hello" `LBS.append` ", World!\n"
  LBS.putStr $ "Hello" <> ", World!\n"

bsconcat3 :: IO ()
bsconcat3 = do
  LBS.putStr $ "Hello" <> ", " <> (LBS.pack $ encode $ show 101) <> " World! " <> (LBS.pack $ encode $ show 0.123) <> "\n"
  LBS.putStr $ BSB.toLazyByteString $ "Hello, " <> BSB.intDec 101 <> " World! " <> BSB.floatDec 0.123 <> "\n"
  where
    encode = fmap (fromIntegral . fromEnum)

bsconcat4 :: IO ()
bsconcat4 = LBS.putStr $ BSB.toLazyByteString $ execWriter f
  where
    f :: Writer BSB.Builder ()
    f = do
      tell "Hello\n"
      tell "Hello\n"
      tell (BSB.intDec 1001) >> tell "\n"
      tell (BSB.wordHex 1024) >> tell "\n"
      tell (BSB.floatDec 0.123) >> tell "\n"
      mapM_ (\i -> tell "Hello" >> tell (BSB.intDec i) >> tell "\n") [0..9] 
      tell "World!\n"
      tell "World!\n"
      tell "\n"

bsbinary1 :: IO ()
bsbinary1 = LBS.putStr $ BSB.toLazyByteString $ execWriter f
  where
    f :: Writer BSB.Builder ()
    f = do
      tell $ BSB.int8 72  -- 'H'
      tell $ BSB.int8 101 -- 'e'
      tell $ BSB.int8 108 -- 'l'
      tell $ BSB.int8 108 -- 'l'
      tell $ BSB.int8 111 -- 'o'
      tell $ BSB.int8 10  -- '\n'
      
      tell $ BSB.int16BE $ ( 72 `shift` 8) + 101 -- 'He'
      tell $ BSB.int8 108 -- 'l'
      tell $ BSB.int16LE $ (111 `shift` 8) + 108 -- 'lo'
      tell $ BSB.int8 10  -- '\n'
      
      tell $ BSB.int32BE $ (72 `shift` 24) + (101 `shift` 16)  + (108 `shift` 8)  + 108 -- 'Hell'
      tell $ BSB.int8 111 -- 'o'
      tell $ BSB.int8 10  -- '\n'

      tell $ BSB.int32LE $ (108 `shift` 24) + (108 `shift` 16)  + (101 `shift` 8)  + 72 -- 'Hell'
      tell $ BSB.int8 111 -- 'o'
      tell $ BSB.int8 10  -- '\n'


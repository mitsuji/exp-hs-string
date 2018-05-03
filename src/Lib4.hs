module Lib4
       (
       ) where

import Data.Monoid




dual1 = getDual $ (Dual [5]) <> (Dual [2]) <> (Dual [5]) <> (Dual [3]) <> (Dual [4])
dual2 = getDual $ mconcat $ fmap Dual $ ([[5],[2],[5],[3],[4]]::[[Int]])


endo1 :: Int -> Int
endo1 = appEndo $ (Endo (+3)) <> (Endo (*2)) <> (Endo (subtract 10) )

endo2 :: Int -> Int
endo2  = appEndo $ mconcat $ fmap Endo $ [(+3), (*2), (subtract 10)]

all1  = getAll $ (All True) <> (All True) <> (All True) <> (All True)
all1' = getAll $ (All True) <> (All True) <> (All False) <> (All True)
all2  = getAll $ mconcat $ fmap (All . (>2)) [3,4,5,4,3]
all2' = getAll $ mconcat $ fmap (All . (>2)) [3,4,2,4,3]


any1  = getAny $ (Any False) <> (Any True) <> (Any False) <> (Any False)
any1' = getAny $ (Any False) <> (Any False) <> (Any False) <> (Any False)
any2  = getAny $ mconcat $ fmap (Any . (<3)) [3,4,5,4,3]
any2' = getAny $ mconcat $ fmap (Any . (<3)) [3,4,2,4,3]


sum1 = getSum $ (Sum 1) <> (Sum 2) <> (Sum 5)
sum2 = getSum $ mconcat $ fmap Sum [1,2,5]

product1 = getProduct $ (Product 1) <> (Product 2) <> (Product 5)
product2 = getProduct $ mconcat $ fmap Product [1,2,5]


-- semigroup のときと違う
--   semigroup 何でも入る     ただ最初や最後に評価される
--   monoid    Maybe を入れる 最初や最後のJustに評価される
first1 = getFirst $ (First Nothing) <> (First $ Just 2) <> (First $ Just 5) <> (First Nothing) <> (First $ Just 4)
first2 = getFirst $ mconcat $ fmap First $ ([Nothing, Just 2, Just 5, Nothing, Just 4]::[Maybe Int])


last1 = getLast $ (Last Nothing) <> (Last $ Just 2) <> (Last $ Just 5) <> (Last $ Just 4) <> (Last Nothing)
last2 = getLast $ mconcat $ fmap Last $ ([Nothing, Just 2, Just 5, Just 4, Nothing]::[Maybe Int])



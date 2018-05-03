module Lib3
       (
       ) where

import Data.Semigroup
import Data.List.NonEmpty (fromList)


min1 = getMin $ (Min 5) <> (Min 2) <> (Min 5) <> (Min 3) <> (Min 4)
min2 = getMin $ sconcat $ fmap Min $ fromList ([5,2,5,3,4]::[Int])


max1 = getMax $ (Max 5) <> (Max 2) <> (Max 5) <> (Max 3) <> (Max 4)
max2 = getMax $ sconcat $ fmap Max $ fromList ([5,2,5,3,4]::[Int])


first1 = getFirst $ (First 5) <> (First 2) <> (First 5) <> (First 3) <> (First 4)
first2 = getFirst $ sconcat $ fmap First $ fromList ([5,2,5,3,4]::[ Int])


last1 = getLast $ (Last 5) <> (Last 2) <> (Last 5) <> (Last 3) <> (Last 4)
last2 = getLast $ sconcat $ fmap Last $ fromList ([5,2,5,3,4]::[ Int])


dual1 = getDual $ (Dual [5]) <> (Dual [2]) <> (Dual [5]) <> (Dual [3]) <> (Dual [4])
dual2 = getDual $ sconcat $ fmap Dual $ fromList ([[5],[2],[5],[3],[4]]::[[Int]])


endo1 :: Int -> Int
endo1 = appEndo $ (Endo (+3)) <> (Endo (*2)) <> (Endo (subtract 10) )

endo2 :: Int -> Int
endo2  = appEndo $ sconcat $ fmap Endo $ fromList [(+3), (*2), (subtract 10)]


all1  = getAll $ (All True) <> (All True) <> (All True) <> (All True)
all1' = getAll $ (All True) <> (All True) <> (All False) <> (All True)
all2  = getAll $ sconcat $ fmap (All . (>2)) $ fromList [3,4,5,4,3]
all2' = getAll $ sconcat $ fmap (All . (>2)) $ fromList [3,4,2,4,3]

any1  = getAny $ (Any False) <> (Any True) <> (Any False) <> (Any False)
any1' = getAny $ (Any False) <> (Any False) <> (Any False) <> (Any False)
any2  = getAny $ sconcat $ fmap (Any . (<3)) $ fromList [3,4,5,4,3]
any2' = getAny $ sconcat $ fmap (Any . (<3)) $ fromList [3,4,2,4,3]


sum1 = getSum $ (Sum 1) <> (Sum 2) <> (Sum 5)
sum2 = getSum $ sconcat $ fmap Sum $ fromList [1,2,5]

product1 = getProduct $ (Product 1) <> (Product 2) <> (Product 5)
product2 = getProduct $ sconcat $ fmap Product $ fromList [1,2,5]


option1  = getOption $ (Option $ Just 1)
option1' = getOption $ (Option Nothing)

option2 = option 100 (\n-> n*2) (Option Nothing)
option2'= option 100 (\n-> n*2) (Option $ Just 3)


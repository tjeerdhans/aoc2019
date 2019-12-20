import Data.Char (digitToInt)
import Data.List (group)
-- 165432-707912
-- following the rules, these are my boundaries 166666-699999

range = [166666..699999]

digits :: Integer -> [Int]
digits = map digitToInt . show

checkInc :: (Ord a) => [a] -> Bool
checkInc [] = True
checkInc [x] = True
checkInc (x:y:xs) = x <= y && checkInc (y:xs)

checkAllInc :: (Ord a) => [a] -> Bool
checkAllInc [] = True
checkAllInc [x] = True
checkAllInc (x:y:xs) = x < y && checkAllInc (y:xs)

atLeastOneDigitTwice :: Eq a => [a] -> Bool
atLeastOneDigitTwice xs = (any (\ g -> length g == 2) $ group xs) 

result4a = length [x | x <- range, let d = digits x, checkInc d, not $ checkAllInc d]

result4b = length [x | x <- range, let d = digits x, checkInc d, not $ checkAllInc d, atLeastOneDigitTwice d]
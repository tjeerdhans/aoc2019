
-- Opcode 1 adds together numbers read from two positions and stores the result in a third position. 
-- The three integers immediately after the opcode tell you these three positions - the first two 
-- indicate the positions from which you should read the input values, and the third indicates the 
-- position at which the output should be stored.
-- For example, if your Intcode computer encounters 1,10,20,30, it should read the values at positions 10 and 20, 
-- add those values, and then overwrite the value at position 30 with their sum.

-- Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. 
-- Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.

-- Once you're done processing an opcode, move to the next one by stepping forward 4 positions.

ops :: Int -> Int -> [Int]
ops n v = [1,n,v,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,2,23,13,27,1,10,27,31,2,31,6,35,1,5,35,39,1,39,10,43,2,9,43,47,1,47,5,51,2,51,9,55,1,13,55,59,1,13,59,63,1,6,63,67,2,13,67,71,1,10,71,75,2,13,75,79,1,5,79,83,2,83,9,87,2,87,13,91,1,91,5,95,2,9,95,99,1,99,5,103,1,2,103,107,1,10,107,0,99,2,14,0,0]

calc :: [Int] -> Int -> [Int]
calc xs i
    | (pv xs i) == (-1,-1) = xs
    | otherwise = calc ([if (fst c) == index then (snd c) else xs!!index | index <- [0..length xs-1], let c = pv xs i]) (i+4)

pv :: [Int] -> Int -> (Int, Int)  
pv xs i
    | (xs!!i) == 1 = ( xs!!(i+3), xs!!(xs!!(i+1)) + xs!!(xs!!(i+2)) ) 
    | (xs!!i) == 2 = ( xs!!(i+3), xs!!(xs!!(i+1)) * xs!!(xs!!(i+2)) ) 
    | otherwise = (-1,-1) -- stop when 99

result2a = head $ calc (ops 12 2) 0

result2b = head [100 * n + v | n <- [0..99], v <- [0..99], (head $ calc (ops n v) 0) == 19690720]
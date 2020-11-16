-- For example, suppose you have the following map:
-- 
-- COM)B
-- B)C
-- C)D
-- D)E
-- E)F
-- B)G
-- G)H
-- D)I
-- E)J
-- J)K
-- K)L
-- 
--         G - H       J - K - L
--        /           /
-- COM - B - C - D - E - F
--                \
--                 I
-- 
-- In this visual representation, when two objects are connected by a line, the one on the right directly orbits the one on the left.
-- 
-- Here, we can count the total number of orbits as follows:
-- 
--     D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
--     L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
--     COM orbits nothing.
-- 
-- The total number of direct and indirect orbits in this example is 42.
import Data.Tree
import Data.List

-- run main to get the answer to 6a
main = do
    input <- readFile "06-input.txt"
    print $ orbitSum $ orbitTree (orbits $ words input) (Node "COM" [])


testinput = ["COM)BBB","BBB)CCC","CCC)DDD","DDD)EEE","EEE)FFF","BBB)GGG","GGG)HHH","DDD)III","EEE)JJJ","JJJ)KKK","KKK)LLL"]
testinputB = ["COM)BBB","BBB)CCC","CCC)DDD","DDD)EEE","EEE)FFF","BBB)GGG","GGG)HHH","DDD)III","EEE)JJJ","JJJ)KKK","KKK)LLL","KKK)YOU","III)SAN"]
orbits xs = [(a,b) | x<-xs, let a = take 3 x, let b = drop 4 x]

orbitTree :: Eq a => [(a, a)] -> Tree a -> Tree a
orbitTree [] (Node a f) = Node a f
orbitTree (x:xs) (Node a f)
    | inTree (fst x) (Node a f) = orbitTree xs (insertAfter (fst x) (snd x) (Node a f))
    -- | inTree (snd x) (Node a f) = orbitTree xs (insertBefore (fst x) (snd x) (Node a f))
    | otherwise = orbitTree (xs++[x]) (Node a f)

orbitSum :: Tree a -> Int
orbitSum t = sum [ i * length (l!!i)| let l = levels t,i <- [0..(length l)-1] ]

inTree :: Eq a => a -> Tree a -> Bool
inTree a (Node x f) 
    | a == x = True
    | otherwise = or [ (inTree a y) | y <- f]

insertBefore :: Eq t => t -> t -> Tree t -> Tree t
insertBefore a b (Node x f) -- insert a before b
    | b==x = Node a [Node x f]
    | otherwise = Node x [insertBefore a b y| y<-f]

insertAfter :: Eq t => t -> t -> Tree t -> Tree t
insertAfter a b (Node x f) -- insert b after a
    | a == x = Node x  ([Node b []] ++ f)
    | otherwise = Node x [insertAfter a b y| y<-f]


-- orbitsToSanta (Node x f) = 
orbitSeqTo as a (Node x f)
    | a == x = as
    | otherwise = orbitSeqTo as++[b | (Node b y)<-f, inTree a (Node b y)] a

-- tests
t = Node "a" [Node "b" [Node "d" []], Node "c" [] ] 
test1 = putStr $ drawTree $ insertBefore "x" "a" t
test2 = putStr $ drawTree $ insertBefore "x" "b" t
test3 = putStr $ drawTree $ insertBefore "c" "b" t

test4 =  putStr $ drawTree $ insertAfter "a" "x" t
test5 =  putStr $ drawTree $ insertAfter "d" "e" t

test6 = orbitTree (orbits testinput) (Node "COM" [])
test7 = putStr $ drawTree test6
test8 = orbitTree (orbits testinputB) (Node "COM" [])
test9 = putStr $ drawTree test8
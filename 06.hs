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
--  G - H       J - K - L
-- /           /
-- COM - B - C - D - E - F
--         \
--          I
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

main = do
    input <- readFile "06-input.txt"
    print $ words input

testinput = ["COM)BBB","BBB)CCC","CCC)DDD","DDD)EEE","EEE)FFF","BBB)GGG","GGG)HHH","DDD)III","EEE)JJJ","JJJ)KKK","KKK)LLL"]


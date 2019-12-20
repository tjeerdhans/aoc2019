moduleWeights :: Fractional a => [a]
moduleWeights = [88062,147838,73346,80732,89182,86798,145656,53825,79515,78250,143033,53680,89366,123255,74974,65373,107733,118266,50726,87810,104355,85331,109624,54282,107472,119291,128702,81132,94609,105929,63918,113360,66932,145080,132130,63858,104334,140635,67642,111552,93446,59263,133164,119788,97327,77379,144054,110747,89394,123533,86026,124422,108855,125000,99270,55789,146945,103156,141044,94238,136833,54370,69178,142349,72239,149992,50901,112759,105467,90841,55693,52532,92343,134889,143351,123359,134972,59986,85415,136521,81581,131078,131201,56194,142135,69982,140667,110013,67772,108135,92591,87200,78189,73407,145395,131869,143480,82068,82423,110819]

-- Fuel required to launch a given module is based on its mass. 
-- Specifically, to find the fuel required for a module, take its mass, 
-- divide by three, round down, and subtract 2.

--fuelReq :: (RealFrac a, Real b) => a -> b
fuelReq x = fromIntegral $ ((floor (x/3))-2)

result1 :: Integer
result1  = sum[fuelReq x | x <- moduleWeights]

-- Fuel itself requires fuel just like a module - take its mass, divide by three, 
-- round down, and subtract 2. However, that fuel also requires fuel, and that 
-- fuel requires fuel, and so on. Any mass that would require negative fuel 
-- should instead be treated as if it requires zero fuel; the remaining mass, 
-- if any, is instead handled by wishing really hard, which has no mass and is 
-- outside the scope of this calculation.

-- fuelModule2 :: (Real p, RealFrac q) => p -> q 
fuelModule2 x 
    | (fuelReq x)<1 = 0
    | otherwise = (fuelReq x) + (fuelModule2 $ fuelReq x)

result2 = sum[fuelModule2 x | x <- moduleWeights]

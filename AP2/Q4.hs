distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

isEqTriangle :: [(Double, Double)] -> Bool 
isEqTriangle [] = False
isEqTriangle (a:b:c:[])
    | a == b || b == c || a == c = False
    | abs((distance a b) - (distance a c)) <= 10e-6 && abs((distance a b) - (distance b c)) <= 10e-6 && abs((distance b c) - (distance a c)) <= 10e-6 = True
    | otherwise = False


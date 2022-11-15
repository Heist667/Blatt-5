{-
Rundet eine Gleitkommazahl n auf die k-te Nachkommastelle.
Beispiel: n = 3.4343232, k = 4 sollte 3.4343 ergeben.
Beispiel: n = 8.9385723, k = 2 sollte 8.94 ergeben.
Beispiel: n = 2.9238477, k = 6 sollte 2.923848 ergeben.
-}
roundDecimalPlace :: Double -> Integer -> Double
roundDecimalPlace n k = fromIntegral(round (n*10^k))/ fromIntegral(10^k)
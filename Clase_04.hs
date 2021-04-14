-- (1) sumatoria sobre i, desde 0 hasta n, de 2 potencia-i


f1 :: (Integral b, Num a) => b -> a
f1 n = 1 - 2*(1-2^n)


-- (2) sumatoria sobre i, desde 1 hasta n, de q potencia-i

f2 :: (Integral a, Integral b) => b -> a -> a
f2 n q = (q - q^(n + 1)) `div` (1 - q) 
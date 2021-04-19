-- Funciones base

sumatoria :: Int -> Int 
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)


-- (1) sumatoria sobre i, desde 0 hasta n, de 2 potencia-i

f1 :: Int -> Int 
f1 0 = 1
f1 n = 2^n + f1(n-1)


-- (2) sumatoria sobre i, desde 1 hasta n, de q potencia-i




-- (3) sumatoria sobre i, desde 1 hasta 2n, de q potencia-i




-- (4) sumatoria sobre i, desde n hasta 2n, de q potencia-i

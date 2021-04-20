-- Funciones base

sumatoria :: Int -> Int 
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1) 


-- (1) sumatoria sobre i, desde 0 hasta n, de 2 potencia-i

f1 :: Int -> Int 
f1 0 = 1
f1 n = 2^n + f1(n-1)


-- (2) sumatoria sobre i, desde 1 hasta n, de q potencia-i

f2 :: Int -> Float -> Float
f2 0 q = 0
f2 n q = q^n + f2 (n-1) q 


-- (3) sumatoria sobre i, desde 1 hasta 2n, de q potencia-i

f3 :: Int -> Float -> Float
f3 n q = f2 (2*n) q 


-- (4) sumatoria sobre i, desde n hasta 2n, de q potencia-i

f4 :: Int -> Float -> Float
f4 n q = f3 n q - f2 (n-1) q 


-- (5) Implementar una función eAprox :: Integer -> Float que
-- aproxime el valor del número e a partir de la siguiente
-- sumatoria:

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = 1 / fromIntegral(factorial n) + eAprox (n-1)

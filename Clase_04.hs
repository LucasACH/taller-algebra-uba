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

e :: Float
e = eAprox 10


-- (6) Implementar la siguiente función:

f6 :: Int -> Int -> Int 
f6 0 m = 0 
f6 n m = f6 (n-1) m + round (f2 m (fromIntegral n)) 


-- (7) Implementar una función sumaPotencias q n m que sume todas
-- las potencias de la forma q^a+b con 1 ≤ a ≤ n y 1 ≤ b ≤ m

sumaPotencias :: Float -> Int -> Int -> Float 
sumaPotencias q n 0 = 0
sumaPotencias q n m = sumaPotencias q n (m-1) + q^m*(f2 n q)


-- (8) Implementar una función sumaRacionales n m que sume todos
-- los números racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m.

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = fromIntegral(sumatoria n) / fromIntegral m + sumaRacionales n (m-1)


--------------------- EJERCICIOS DE TAREA ---------------------

-- (1) Implementar la siguiente función:

g1 :: Int -> Int -> Int
g1 i n | i > n = 0
       | otherwise = i^i + g1 (i+1) n


-- (2) Implementar la siguiente función:

g2 :: Int -> Int
g2 0 = 0
g2 n = n^n * n + g2 (n-1)   
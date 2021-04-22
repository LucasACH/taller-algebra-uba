-- (1) Implementar una función sumaDivisoresHasta :: Int -> Int -> Int.

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)


-- (2) Implementar la función sumaDivisores en función de la anterior.

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n 


-- (3) Implementar menorDivisor :: Int -> Int que calcule el menor divisor
-- (mayor que 1) de un natural n.

menorDivisor :: Int -> Int
menorDivisor n | n `mod` 2 == 0 = 2
               | n `mod` 3 == 0 = 3
               | otherwise = n


-- (4) Implementar la función esPrimo :: Int -> Bool.

esPrimo :: Int -> Bool
esPrimo n = (sumaDivisores n) - 1 == n


-- (5) Implementar la función nEsimoPrimo :: Int -> Int que devuelve el n-esi
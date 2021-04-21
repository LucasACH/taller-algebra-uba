-- (1) Implementar una función sumaDivisoresHasta :: Int -> Int -> Int.

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)


-- (2) Implementar la función sumaDivisores en función de la anterior.

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n 

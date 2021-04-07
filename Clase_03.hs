-- (1) Escribir una función para determinar si un número natural es múltiplo de 3. No está permitido utilizar mod ni div.

esMultiplo3 :: Int -> Bool 
esMultiplo3 n | n == 3 = True
              | n < 3 = False
              | otherwise = esMultiplo3 (n - 3) 


-- (2) Implementar la función sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n números impares. Ej: sumaImpares 3 1+3+5 9.

sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 1
              | otherwise = 2 * n - 1 + sumaImpares (n - 1)


-- (3) Escribir una función medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por ejemplo: medioFact 10 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 3840. medioFact 9 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 945.

medioFact :: (Ord a, Num a) => a -> a
medioFact n | n <= 1 = 1
            | otherwise = n * medioFact(n - 2)


-- (4) Escribir una función que determine la suma de dígitos de un número positivo. Para esta función pueden utilizar div y mod.

sumaDigitos :: Int -> Int
sumaDigitos n | n < 10 = n
              | otherwise =  n `mod` 10 + sumaDigitos(n `div` 10)


-- (5) Implementar una función que determine si todos los dígitos de un número son iguales.

todosIguales :: Int -> Bool
todosIguales n | n < 10 = True 
               | (n `mod` 100) `mod` 10 == (n `mod` 100) `div` 10 = todosIguales(n `div` 10)
               | otherwise = False
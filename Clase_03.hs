-- (1) Escribir una función para determinar si un número natural es múltiplo de 3. No está permitido utilizar mod ni div.

esMultiplo3 :: Int -> Bool 
esMultiplo3 n  | n == 3 = True
               | n < 3 = False
               | otherwise = esMultiplo3 (n - 3) 
               -- La función se sigue ejecutando hasta llegar a 3, si se cumple que n es multipo de 3


-- (2) Implementar la función sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n números impares. Ej: sumaImpares 3 1+3+5 9.

sumaImpares :: Int -> Int
sumaImpares n = n^2


-- (3) Escribir una función medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por ejemplo: medioFact 10 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 3840. medioFact 9 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 945.


-- (4) Escribir una función que determine la suma de dígitos de un número positivo. Para esta función pueden utilizar div y mod.


-- (5) Implementar una función que determine si todos los dígitos de un número son iguales.
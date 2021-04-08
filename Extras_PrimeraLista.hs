-- (1) Escriba una función dígito :: Int -> Integer -> Integer tal que la expresión
-- dígito i n calcule el i-ésimo dígito decimal del entero n, asumiendo que n es
-- un entero no negativo y que i es un entero positivo.

digito :: Int -> Integer -> Integer
digito i n = (n `mod` 10^i) `div` 10^(i-1)


-- (2) Escriba una función sumaDeDígitos :: Integer -> Integer tal que la expresión
-- sumaDeDígitos n sea la suma de los dígitos del entero n, que asumimos es no
-- negativo.

sumaDeDigitos :: Integer -> Integer
sumaDeDigitos n | n < 10 = n
                | otherwise = n `mod` 10 + sumaDeDigitos(n `div` 10)


-- (3) Escriba una función dígitoMáximo :: Integer -> Integer tal que la expresión
-- dígitoMáximo n sea dígito más grande de n, que asumimos es no negativo.

-- digitoMaximo :: Integer -> Integer


-- (4) Escriba una función factorial :: Integer -> Integer tal que factorial n sea el
-- factorial del entero no negativo n

factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)


-- (5) Escriba una función e_approx :: Integer -> Double tal que para cada entero no
-- negativo n el valor de e_approx n sea

factorialDouble :: Double -> Double
factorialDouble n | n == 0 = 1
                  | n > 0 = n * factorialDouble (n-1)

e_approx :: Double -> Double
e_approx n | n == 0 = 1
           | otherwise = 1 / factorialDouble n + e_approx(n-1)


-- (6) Escriba una función unos :: Integer -> Integer tal que para cada entero no
-- negativo n el valor de unos n sea la cantidad de dígitos 1 en la escritura binaria
-- de n.

unos :: Integer -> Integer
unos 1 = 1
unos n | n < 1 = 0
       | mod2 == 0 = unos(div2)
       | otherwise = 1 + unos(div2)

       where div2 = n `div` 2
             mod2 = n `mod` 2
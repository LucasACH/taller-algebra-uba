-- (1) Escriba una función dígito :: Int -> Integer -> Integer tal que la expresión
-- dígito i n calcule el i-ésimo dígito decimal del entero n, asumiendo que n es
-- un entero no negativo y que i es un entero positivo.

digito :: Int -> Integer -> Integer
digito i n = (n `mod` 10^i) `div` 10^(i-1)


-- (2) Escriba una función sumaDeDígitos :: Integer -> Integer tal que la expresión
-- sumaDeDígitos n sea la suma de los dígitos del entero n, que asumimos es no
-- negativo.

sumaDeDigitos :: Integer -> Integer
sumaDeDigitos i | i < 10 = i
                | otherwise = i `mod` 10 + sumaDeDigitos(i `div` 10)


-- (3) Escriba una función dígitoMáximo :: Integer -> Integer tal que la expresión
-- dígitoMáximo n sea dígito más grande de n, que asumimos es no negativo.

-- digitoMaximo :: Integer -> Integer


-- (4) Escriba una función factorial :: Integer -> Integer tal que factorial n sea el
-- factorial del entero no negativo n

factorial :: Integer -> Integer
factorial i | i == 0 = 1
            | i > 0 = i * factorial (i-1)


-- (5) Escriba una función e_approx :: Integer -> Double tal que para cada entero no
-- negativo n el valor de e_approx n sea
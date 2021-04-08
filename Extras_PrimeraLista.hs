-- 1. Escriba una función dígito :: Int -> Integer -> Integer tal que la expresión
-- dígito i n calcule el i-ésimo dígito decimal del entero n, asumiendo que n es
-- un entero no negativo y que i es un entero positivo.

digito :: Int -> Integer -> Integer
digito i n = (n `mod` 10^i) `div` 10^(i-1)
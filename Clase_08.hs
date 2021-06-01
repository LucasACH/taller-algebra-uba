factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) 


-- (1) Escribir una función que dados n, k ∈ N tal que 0 ≤ k ≤ n, compute el combinatorio (n | k)

combinatorio :: Int -> Int -> Int
combinatorio n k = factorial (n) `div` (factorial(k) * factorial(n-k))

--  Ahora usando la igualdad (n | k) = ((n - 1) | k) + ((n - 1) | (k - 1)) para 1 ≤ k ≤ n

combinatorio' :: Int -> Int -> Int
combinatorio' n 0 = 1
combinatorio' n k | n == k = 1
                  | otherwise = (combinatorio' (n - 1) k) + (combinatorio' (n - 1) (k - 1))


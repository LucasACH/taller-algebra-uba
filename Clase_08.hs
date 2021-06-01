type Set a = [a]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) 

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)


-- (1) Escribir una función que dados n, k ∈ N tal que 0 ≤ k ≤ n, compute el combinatorio (n | k)

combinatorio :: Int -> Int -> Int
combinatorio n k = factorial (n) `div` (factorial(k) * factorial(n-k))

--  Ahora usando la igualdad (n | k) = ((n - 1) | k) + ((n - 1) | (k - 1)) para 1 ≤ k ≤ n

combinatorio' :: Int -> Int -> Int
combinatorio' n 0 = 1
combinatorio' n k | n == k = 1
                  | otherwise = (combinatorio' (n - 1) k) + (combinatorio' (n - 1) (k - 1))


-- (2) Implementar una función variaciones :: Set Int -> Int -> Set [Int] que dado un conjunto c y una longitud k genere todas las posibles listas de longitud k a partir de elementos de c.

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante x [      ] = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [    ] _ = []
agregarElementosAListas (x:xs) c = (agregarElementoAdelante x c) `union` (agregarElementosAListas xs c)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k - 1))

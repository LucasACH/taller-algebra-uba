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


-- (3) Implementar una función insertarEn :: [Int] -> Int -> Int -> [Int] que dados una lista l, un número n y una posición i (contando desde 1) devuelva una lista en donde se insertó n en la posición i de l y los elementos siguientes corridos en una posición.

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn xs n i | i == 1 = n:xs 
                  | otherwise = (head xs) : (insertarEn (tail xs) n (i - 1)) 


-- (4) Implementar una función permutaciones :: Set Int -> Set [Int] que dado un conjunto de enteros, genere todas las posibles permutaciones de los números del conjunto pasado por parámetro.

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i - 1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [      ] c = []    
insertarEnCadaPosDeTodasLasListas (xs:xss) c = (insertarEnCadaPos xs c (length xs + 1)) `union` (insertarEnCadaPosDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int]
permutaciones [    ] = [[]]
permutaciones (x:xs) =  insertarEnCadaPosDeTodasLasListas (permutaciones xs) x


-- (5) Todas las formas de ubicar n bolitas numeradas en k cajas.

listaEnterosHasta :: Int -> [Int]
listaEnterosHasta 0 = []
listaEnterosHasta n = n : listaEnterosHasta (n - 1)

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = variaciones ((listaEnterosHasta k) `union` (listaEnterosHasta n)) (length (listaEnterosHasta n))
                

-- (6) Todas las formas de ubicar n bolitas numeradas en k cajas tal que la primera caja nunca esté vacía.

formasDeUbicarBolitas :: Int -> Int -> Int
formasDeUbicarBolitas n k = factorial k `div` factorial (k - n)
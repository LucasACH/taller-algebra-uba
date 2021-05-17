pertenece :: Int -> [Int] -> Bool
pertenece n l | l == [] = False
              | otherwise = (n == head l) || pertenece n (tail l)

cantidadDeApariciones :: Int -> [Int] -> Int
cantidadDeApariciones n [] = 0
cantidadDeApariciones n l | n == head l = 1 + cantidadDeApariciones n (tail l)
                          | otherwise = cantidadDeApariciones n (tail l)


-- (1) productoria :: [Int] -> Int que devuelve la productoria de los elementos.

productoria :: [Int] -> Int
productoria [] = 1
productoria (x : xs) = x * productoria xs


-- (2) sumarN :: Int -> [Int] -> [Int] que dado un número N y una lista xs, suma N a cada
-- elemento de xs.

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = (x + n) : sumarN n xs


-- (3) sumarElPrimero :: [Int] -> [Int] que dada una lista no vacía xs, suma el primer
-- elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3] [2,3,4]

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero l = sumarN (head l) l  


-- (4) sumarElUltimo :: [Int] -> [Int] que dada una lista no vacía xs, suma el último
-- elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3] [4,5,6]

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = sumarN (last l) l


-- (5) pares :: [Int] -> [Int] que devuelve una lista con los elementos pares de la lista
-- original. Ejemplo pares [1,2,3,5,8] [2,8]

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | even x = x : pares xs
             | otherwise = pares xs   


-- (6) quitar :: Int -> [Int] -> [Int] que elimina la primera aparición del elemento en la
-- lista (de haberla).

quitar :: Int -> [Int] -> [Int]
quitar n [] = []
quitar n l | cantidadDeApariciones n l > 1 && n == head l = tail l
           | otherwise = (head l) : quitar n (tail l)


-- (7) quitarTodas :: Int -> [Int] -> [Int] que elimina todas las apariciones del elemento
-- en la lista (de haberla).

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n [] = []
quitarTodas n l | cantidadDeApariciones n l > 0 && n == head l = quitarTodas n (tail l)
                | otherwise = (head l) : quitarTodas n (tail l)


-- (8) hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos repetidos.

-- (9) eliminarRepetidosAlFinal :: [Int] -> [Int] que deja en la lista la primera aparición
-- de cada elemento, eliminando las repeticiones adicionales.

-- (10) eliminarRepetidosAlInicio :: [Int] -> [Int] que deja en la lista la última aparición
-- de cada elemento, eliminando las repeticiones adicionales.

-- (11) maximo :: [Int] -> Int que calcula el máximo elemento de una lista no vacía.
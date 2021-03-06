pertenece :: Int -> [Int] -> Bool
pertenece n l | l == [] = False
              | otherwise = (n == head l) || pertenece n (tail l)

cantidadDeApariciones :: Int -> [Int] -> Int
cantidadDeApariciones n [] = 0
cantidadDeApariciones n (x:xs) | n == x = 1 + cantidadDeApariciones n (xs)
                               | otherwise = cantidadDeApariciones n (xs)


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
quitar n l | head l == n = tail l
           | otherwise = head l : (quitar n (tail l))


-- (7) quitarTodas :: Int -> [Int] -> [Int] que elimina todas las apariciones del elemento
-- en la lista (de haberla).

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n [] = []
quitarTodas n l | cantidadDeApariciones n l > 0 && n == head l = quitarTodas n (tail l)
                | otherwise = (head l) : quitarTodas n (tail l)


-- (8) hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos repetidos.

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = cantidadDeApariciones x xs == 1 || hayRepetidos xs
                  
               
-- (9) eliminarRepetidosAlFinal :: [Int] -> [Int] que deja en la lista la primera aparición
-- de cada elemento, eliminando las repeticiones adicionales.

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs) = x : quitarTodas x (eliminarRepetidosAlFinal (xs)) 
                           

-- (10) eliminarRepetidosAlInicio :: [Int] -> [Int] que deja en la lista la última aparición
-- de cada elemento, eliminando las repeticiones adicionales.

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (x:xs) | cantidadDeApariciones x xs > 0 = (eliminarRepetidosAlInicio (xs))
                                 | otherwise = x : (eliminarRepetidosAlInicio (xs))

-- (11) maximo :: [Int] -> Int que calcula el máximo elemento de una lista no vacía.

maximo :: [Int] -> Int
maximo (x:[]) = x
maximo (x:xs) | x > head xs = maximo (x : tail xs)
              | otherwise = maximo xs


-- (12) ordenar :: [Int] -> [Int] que ordena los elementos de forma creciente.

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = max : (ordenar (quitarTodas (max) l))
          where
              max = maximo l


-- (13) reverso :: [Int] -> [Int] que dada una lista invierte su orden.

reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]


-- (14) concatenar :: [Int] -> [Int] -> [Int] que devuelve la concatenación de la primera
-- lista con la segunda. Ejemplo concatenar [1,2,3] [4,5,6] [1,2,3,4,5,6],
-- concatenar [] [4,5,6] [4,5,6]. Esta operación está en el prelude y se escribe como
-- (++).

concatenar :: [Int] -> [Int] -> [Int]
concatenar a b = a ++ b

-- (15) zipi :: [a] -> [b] -> [(a,b)] que devuelve una lista de tuplas, cada tupla contiene
-- elementos de ambas listas que ocurren en la misma posición. En caso que tengan distintas
-- longitudes, la longitud de la lista resultado es igual a la longitud de la lista más chica
-- pasada por parámetro. Ejemplo zipi [1,2,3] ['a','b','c']
-- [(1,'a'), (2,'b'), (3,'c')], zipi [1,2,3] ['a','b'] [(1,'a'), (2,'b')]. Esta
-- operación está en el prelude y se escribe como zip.

zipi :: [a] -> [b] -> [(a,b)]
zipi [] b = []
zipi a [] = []
zipi (a:as) (b:bs) = (a,b) : (zipi (as) (bs)) 
largoLista :: [Int] -> Int
largoLista l | l == [] = 0
             | otherwise = 1 + largoLista( tail(l) )


-- (1) productoria :: [Int] -> Int que devuelve la productoria de los elementos.

productoria :: [Int] -> Int
productoria l | largoLista l == 1 = head l
productoria l = head l * productoria( tail l )


-- (2) sumarN :: Int -> [Int] -> [Int] que dado un número N y una lista xs, suma N a cada
-- elemento de xs.

-- (3) sumarElPrimero :: [Int] -> [Int] que dada una lista no vacía xs, suma el primer
-- elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3] [2,3,4]

-- (4) sumarElUltimo :: [Int] -> [Int] que dada una lista no vacía xs, suma el último
-- elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3] [4,5,6]

-- (5) pares :: [Int] -> [Int] que devuelve una lista con los elementos pares de la lista
-- original. Ejemplo pares [1,2,3,5,8] [2,8]

-- (6) quitar :: Int -> [Int] -> [Int] que elimina la primera aparición del elemento en la
-- lista (de haberla).

-- (7) quitarTodas :: Int -> [Int] -> [Int] que elimina todas las apariciones del elemento
-- en la lista (de haberla).

-- (8) hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos repetidos.

-- (9) eliminarRepetidosAlFinal :: [Int] -> [Int] que deja en la lista la primera aparición
-- de cada elemento, eliminando las repeticiones adicionales.

-- (10) eliminarRepetidosAlInicio :: [Int] -> [Int] que deja en la lista la última aparición
-- de cada elemento, eliminando las repeticiones adicionales.

-- (11) maximo :: [Int] -> Int que calcula el máximo elemento de una lista no vacía.
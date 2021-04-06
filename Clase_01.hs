-- Ejercicios primera clase https://campus.exactas.uba.ar/pluginfile.php/267823/mod_resource/content/1/clase01.pdf

-- Funciones base

maximo :: Int -> Int -> Int 
maximo x y | x > y = x
           | otherwise = y


-- (1) absoluto: calcula el valor absoluto de un número entero.

absoluto :: Int -> Int 
absoluto n = maximo n (-n)


-- (2) maximoabsoluto: devuelve el máximo entre el valor absoluto de dos números enteros.

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y = maximo (absoluto x) (absoluto y)


-- (3) maximo3: devuelve el máximo entre tres números enteros.

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = maximo z (maximo x y)
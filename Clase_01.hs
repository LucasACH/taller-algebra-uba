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


-- (4) algunoEs0: dados dos números racionales, decide si alguno de los dos es igual a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = x * y == 0 -- Sin pattern matching

algunoEs0_ :: Float -> Float -> Bool
algunoEs0_ x y = x == 0 || y == 0 -- Con pattern matching


-- (5) ambosSon0: dados dos números racionales, decide si ambos son iguales a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).

ambosSon0 :: Int -> Int -> Bool
ambosSon0 x y = x + y == 0 -- Sin pattern matching

ambosSon0_ :: Int -> Int -> Bool
ambosSon0_ x y = x == 0 && y == 0 -- Con pattern matching


-- (6) esMultiploDe: dados dos números naturales, decidir si el primero es múltiplo del segundo.

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = y `mod` x == 0


-- (7) digitoUnidades: dado un número natural, extrae su dígito de las unidades.

digitoUnidades :: Int -> Int 
digitoUnidades n = n `mod` 10


-- (8) digitoDecenas: dado un número natural, extrae su dígito de las decenas.

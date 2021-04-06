-- Ejercicios primera clase https://campus.exactas.uba.ar/pluginfile.php/267823/mod_resource/content/1/clase01.pdf


-- (1) absoluto: calcula el valor absoluto de un número entero.

absoluto :: Int -> Int 
absoluto n | n < 0 = -n
           | otherwise = n

-- (2) maximoabsoluto: devuelve el m´aximo entre el valor absoluto de dos n´umeros enteros.

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y | absoluto x > absoluto y = absoluto x
                   | otherwise = absoluto y
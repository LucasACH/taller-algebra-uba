-- Ejercicios primera clase https://campus.exactas.uba.ar/pluginfile.php/267823/mod_resource/content/1/clase01.pdf

-- (1) absoluto: calcula el valor absoluto de un número entero.

absoluto :: Int -> Int 
absoluto n | n < 0 = -n
           | otherwise = n


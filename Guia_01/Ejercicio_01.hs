-- absoluto: calcula el valor absoluto de un número entero.

absoluto :: Int -> Int 
absoluto n | n < 0 = -n
           | otherwise = n

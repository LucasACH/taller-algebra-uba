-- (1) Definir la función digitos :: Integer -> Integer -> [Integer] que, dados n ≥ 0 y b > 1, retorne su representación por listas en base b.

digitos :: Integer -> Integer -> [Integer]
digitos 0 b = []
digitos n b = n `mod` b : digitos (n `div` b) b


-- (2) Definir la función numero :: [Integer] -> Integer -> Integer que, dada la representación por listas de n ≥ 0 en base b y la base b > 1, retorne n.

-- (3) Escribir la función divisores :: Int -> Set Int que dado un valor n != 0 retorna el conjunto de sus divisores positivos

-- (4) Completar la función mcdDef, definiendo las funciones restantes

-- (5) Medir el tiempo que tarda mcdDef para un par de valores en 1010 ≤ a, b ≤ 2 · 1010

-- (7) Definir la función mcd :: Int -> Int -> Int que dados a, b ∈ Z, b != 0, calcule (a : b) usando el algoritmo de Euclides.

-- (7) Medir el tiempo de esta función y compararlo con mcdDef.

-- (8) Definir un función mcm :: Int -> Int -> Int que dados a ≥ 0 y b ≥ 0 calcule el mínimo d ≥ 0 que sea múltiplo tanto de a como de b. ¿Cuánto vale mcm 0 0?

-- (9) Programar la función emcd :: Int -> Int -> (Int, Int, Int) que, dados a y b, utilice el algoritmo de Euclides extendido para obtener una tripla ((a : b), s,t) tal que sa + tb = (a : b)

-- (10) Definir una función que dados a != 0 y b != 0 encuentre el par s,t ∈ Z tal que sa + tb = (a : b) donde s ≥ 0 sea lo mínimo posible. Repasar la teórica para este ejercicio.
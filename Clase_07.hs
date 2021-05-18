type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = xs == ys || perteneceC xs yss

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c 
            | otherwise = x : c

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss 
               | otherwise = xs : xss

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c  


-- (1) union:: Set Int -> Set Int -> Set Int que dado dos conjuntos, devuelve la unión
-- entre ellos.

union :: Set Int -> Set Int -> Set Int
union [] c = c
union (x:xs) c | pertenece x c = union xs c
               | otherwise = x : (union xs c)

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c = c
unionC (x:xs) c | perteneceC x c = unionC xs c
                | otherwise = x : (unionC xs c)


-- (2) interseccion :: Set Int -> Set Int -> Set Int que dado dos conjuntos, devuelve la
-- interesección entre ellos.

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] c = []
interseccion (x:xs) c | pertenece x c = x : interseccion xs c
                       | otherwise = interseccion xs c


-- (3) diferencia :: Set Int -> Set Int -> Set Int que dado los conjuntos A y B, devuelve
-- A \ B.

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] c = []
diferencia x [] = []
diferencia (x:xs) c | pertenece x c = diferencia xs c
                    | otherwise = x : diferencia xs c

-- (4) diferenciaSimetrica :: Set Int -> Set Int -> Set Int que dado los conjuntos A y
-- B, devuelve la diferencia simétrica, es decir, A4B.

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica x c =  union (diferencia x c) (diferencia c x)


-- (5) Implementar una función
-- partes :: Set Int -> Set (Set Int) que genere el conjunto de partes de un conjunto
-- dado.

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs) 

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))


-- (6) partesN :: Int -> Set (Set Int) que genere los subconjuntos del conjunto
-- {1, 2, 3, . . . , n}.

partesN :: Int -> Set (Set Int)
partesN n = partes [1..n]


-- (7) productoCartesiano :: Set Int -> Set Int -> Set (Int, Int) que dados dos
-- conjuntos genere todos los pares posibles (como pares de dos elementos) tomando el primer
-- elemento del primer conjunto y el segundo elemento del segundo conjunto.

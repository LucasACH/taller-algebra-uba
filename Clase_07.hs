type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c 
            | otherwise = x : c

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c  


-- (1) union:: Set Int -> Set Int -> Set Int que dado dos conjuntos, devuelve la unión
-- entre ellos.

union :: Set Int -> Set Int -> Set Int
union [] c = c
union (x:xs) c | pertenece x c = union xs c
               | otherwise = x : (union xs c)


-- (2) intereseccion :: Set Int -> Set Int -> Set Int que dado dos conjuntos, devuelve la
-- interesección entre ellos.

-- (3) diferencia :: Set Int -> Set Int -> Set Int que dado los conjuntos A y B, devuelve
-- A \ B.

-- (4) diferenciaSimetrica :: Set Int -> Set Int -> Set Int que dado los conjuntos A y
-- B, devuelve la diferencia simétrica, es decir, A4B.
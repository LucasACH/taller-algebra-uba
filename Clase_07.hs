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
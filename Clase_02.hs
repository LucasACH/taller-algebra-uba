-- (1) estanRelacionados: dados dos números reales, decide si están relacionados considerando la relación de equivalencia en R cuyas clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞).

estanRelacionados :: Int -> Int -> Bool 
estanRelacionados x y | x < 4 && y < 4 = True
                      | (x > 3 && y > 3) && (x < 8 && y < 8) = True     
                      | x > 7 && y > 7 = True
                      | otherwise = False


-- (2) prodInt: calcula el producto interno entre dos vectores de R2.

prodInt :: Num a => (a, a) -> (a, a) -> a
prodInt (ux, uy) (vx, vy) = ux * vx + uy * vy


-- (3) todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer vector es menor a la coordenada correspondiente del segundo vector.

todoMenor :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
todoMenor (ux, uy) (vx, vy) = ux < vx && uy < vy


-- (4) distanciaPuntos: calcula la distancia entre dos puntos de R2.

distanciaPuntos :: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos (px, py) (qx, qy) = sqrt((qx - px)^2 + (qy - py)^2)


-- (5) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.

sumaTerna :: Num a => (a, a, a) -> a
sumaTerna (px, py, pz) = px + py + pz


-- (6) posicPrimerPar: dada una terna de enteros, devuelve la posición del primer número par si es que hay alguno, y devuelve 4 si son todos impares.

posicPrimerPar :: Integral a => (a, a, a) -> Int 
posicPrimerPar (px, py, pz) | px `mod` 2 == 0 = 1
                            | py `mod` 2 == 0 = 2
                            | pz `mod` 2 == 0 = 3
                            | otherwise = 4


-- (7) crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por separado (debe funcionar para elementos de cualquier tipo).

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)


-- (8) invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como parámetro (debe funcionar para elementos de cualquier tipo).
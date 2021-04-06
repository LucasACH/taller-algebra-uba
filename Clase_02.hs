-- (1) estanRelacionados: dados dos números reales, decide si están relacionados considerando la relación de equivalencia en R cuyas clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞).

estanRelacionados :: Int -> Int -> Bool 
estanRelacionados x y | x < 4 && y < 4 = True
                      | (x > 3 && y > 3) && (x < 8 && y < 8) = True     
                      | x > 7 && y > 7 = True
                      | otherwise = False


-- (2) prodInt: calcula el producto interno entre dos vectores de R2.



-- (3) todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer vector es menor a la coordenada correspondiente del segundo vector.



-- (4) distanciaPuntos: calcula la distancia entre dos puntos de R2.



-- (5) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.



-- (6) posicPrimerPar: dada una terna de enteros, devuelve la posición del primer número par si es que hay alguno, y devuelve 4 si son todos impares.



-- (7) crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por separado (debe funcionar para elementos de cualquier tipo).



-- (8) invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como parámetro (debe funcionar para elementos de cualquier tipo).
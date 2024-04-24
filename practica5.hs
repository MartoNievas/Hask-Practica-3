-- Ejercicio 1

-- a 

longitud :: [t] -> Integer 
longitud [] = 0 
longitud (x:xs) = 1 + longitud xs 

-- b

ultimo :: [t] -> t
ultimo [] = error "Lista Vacia" 
ultimo [x] = x 
ultimo (x:xs) = ultimo xs 

-- c 

principio :: [t] -> [t]
principio (x:[]) = []
principio (x:xs) = x : principio xs 

-- d

reverso :: [t] -> [t]
reverso [] = []
reverso xs = ultimo xs : reverso ( principio xs ) 

-- Ejercicio 2 

-- a 

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False 
pertenece x (y:ys) = x == y || pertenece x ys

-- b 

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True 
todosIguales (x:xs) = x == head xs && todosIguales (xs)

-- c 

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True 
todosDistintos [x] = True  
todosDistintos (x:xs) = pertenece x xs == False

-- d 

hayRepetidos :: (Eq t) => [t] -> Bool 
hayRepetidos [] = False 
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

-- e 

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys) | x == y = ys 
                | otherwise = y : quitar x ys 

-- 1 [1,1,2,3,4,5] = [1,2,3,4,5] 
-- 1 [2,3,45,1] 
-- f 

quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs 
                     | otherwise = x : quitarTodos e xs 

-- h 

eliminarRepetidos :: (Eq t) => [t] -> [t] 
eliminarRepetidos [] = [] 
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs )

-- i 

estaContenida :: (Eq t) => [t] -> [t] -> Bool 
estaContenida [] ys = True 
estaContenida (x:xs) ys = pertenece x ys && estaContenida xs ys   


mismosElementos :: (Eq t) => [t] -> [t] -> Bool 
mismosElementos xs ys = estaContenida xs ys && estaContenida ys xs 

-- j 

capicua :: (Eq t) => [t] -> Bool 
capicua xs = xs == reverso xs 

-- Ejercicio 3 

-- a

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs 
 
-- b

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- c

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) | x > y = maximo (x:xs)
                | otherwise = maximo (y:xs) 

-- d 

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) = n + x : sumarN n xs  

-- e 

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x (x:xs) 

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo (x:xs) = sumarN (ultimo xs) (x:xs)

-- f

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) 
    | even x = x : pares xs 
    | otherwise = pares xs 

-- g 

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs 
                      | otherwise = multiplosDeN n xs 

-- h 

ordenar :: [Integer] -> [Integer]
ordenar [x] = [x]
ordenar xs = minimo xs : ordenar (quitar (minimo xs ) xs )


minimo :: [Integer] -> Integer
minimo [x] = x
minimo (x:y:xs) | x < y = minimo (x:xs)
                | otherwise = minimo (y:xs) 
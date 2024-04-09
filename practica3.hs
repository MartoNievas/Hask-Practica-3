module Uno where 

--Funcion f que le meto un entero y devuelve otro 

f :: Int -> Int 
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16

-- Funcion f que le meto un entero y devuelve otro

g :: Int -> Int 
g n | n == 8 = 16 
    | n == 16 = 4 
    | n == 131 = 1

    -- funciones compuestas h y k 

h1::Int -> Int
h1 x = f (g x)

k1 :: Int -> Int
k1 z = g (f z)


{- EJERCICIO 2 -}

-- A
absoluto :: Int -> Int
absoluto n = abs n

-- B
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | abs x > abs y = abs x
                | otherwise = abs y

-- C
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
            | y > x && y > z = y
            | otherwise = z

-- D
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 || y == 0 = True
            | otherwise = False

algunoEs0PM :: Float -> Float -> Bool
algunoEs0PM x y = x == 0 || y == 0

-- E
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = x == 0 && y == 0

-- F
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y = (x <= 3 && y <= 3) || (x >= 7 && y >= 7) || ((x > 3 && x <= 7) && (y > 3 && y <= 7))

-- G
filterNonRepeated :: Eq a => [a] -> [a]
filterNonRepeated [] = []
filterNonRepeated (x:xs)
    | x `elem` xs = filterNonRepeated (filter (/= x) xs) -- If x is repeated, filter it out
    | otherwise = x : filterNonRepeated xs             -- Otherwise, keep it

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z = sum . filterNonRepeated $ [x, y, z]

-- H
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = x `mod` y == 0

-- I
digitoUnidades :: Int -> Int 
digitoUnidades x = abs (x `mod` 10) 

-- J
digitoDecenas :: Int -> Int 
digitoDecenas x = digitoUnidades(x `div` 10)

{- EJERCICIO 4  -}
-- C

prodInt :: (Float,Float) -> (Float,Float) -> (Float,Float)
prodInt (a,b) (c,d) = (a * c, b * d)

-- B
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor x y = fst x < fst y && snd x < snd y

--Ejercicio con Pattern Matching

todoMenorPM :: (Float,Float) -> (Float,Float) -> Bool
todoMenorPM (a,b) (c,d) = a < c && b < d 

-- C
 
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt ((a - c) + (b - d))

-- D 

sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (a,b,c) = a + b + c

-- E

sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int
sumarSoloMultiplos (a,b,c) d | mod a b == 0 && mod b c == 0 && mod c d == 0 = (a + b + c)   
                             | mod a b == 0 && mod b c == 0 && mod c d /= 0 = a + b 
                             | mod a b == 0 && mod b c /= 0 && mod c d == 0 = a + c
                             | mod a b /= 0 && mod b c == 0 && mod c d == 0 = b + c 
                             | mod a b == 0 && mod b c /= 0 && mod c d /= 0 = a 
                             | mod a b /= 0 && mod b c == 0 && mod c d /= 0 = b
                             | mod a b /= 0 && mod b c /= 0 && mod c d == 0 = c 
                             | otherwise = 0

-- F nota el comando even verifica si el elemento es par mas simple que mod 

posPrimerPar :: (Int,Int, Int) -> Int
posPrimerPar (a,b,c) | mod a 2 == 0 = 0
                     | mod b 2 == 0 = 1
                     | mod c 2 == 0 = 2
                     | otherwise = 4           

-- G

crearPar:: a -> b-> (a,b)
crearPar a b = (a,b)

-- H 

invertirPar :: (a,b) -> (b,a)
invertirPar (a,b) = (b,a)


-- Ejercicio 3 

estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b | mod a b == 0 = True
                      | otherwise = False
                                        
-- Ejercicio 5 

todosMenores :: (Int,Int,Int) -> Bool
todosMenores (a,b,c) = f a > g a && f b > g b && f c > g c 

problemaF :: Int -> Int
problemaF n | n <= 7 = n^2
            | n > 7 = 2*n - 1

problemaG :: Int -> Int 
problemaG n | even n = div n 2 
            | otherwise = 3*n + 1

-- Ejercicio 6 

bisiesto :: Int -> Bool
bisiesto n | mod n 4 == 0 || mod n 400 == 0 = True 
           | mod n 100 == 0 = False
           | otherwise = False

-- Ejercicio 7 

distanciaManhanttan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhanttan (a,b,c) (d,e,f) = abs (a - d) + abs (b - c) + abs (c - f)

-- Ejercicio 8 

comparar :: Int -> Int -> Int 
comparar a b | mod a 100 < mod b 100 = 1 
             | mod a 100 > mod b 100 = -1 
             | mod a 100 == mod b 100 = 0 

-- Ejercicio 9 

{-  -}
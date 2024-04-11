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
distanciaPuntos (a,b) (c,d) = sqrt ((a - c)² + (b - d)²)

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
todosMenores (a,b,c) = F a > G a && F b > G b && F c > G c 

F :: Int -> Int
F n | n <= 7 = n^2
            | n > 7 = 2*n - 1

G :: Int -> Int 
G n | even n = div n 2 
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
comparar a b | sumaUltimoDosDigitos a > sumaUltimoDosDigitos b = -1
             | sumaUltimoDosDigitos a < sumaUltimoDosDigitos b = 1
             | otherwise = 0

sumaUltimoDosDigitos :: Int -> Int 
sumaUltimoDosDigitos x = digitoDecenas x + digitoUnidades x


-- Ejercicio 9 

{- f1 devuelve 1 si la entrada es 0 y para otro caso devuelve 0
problemaf1 (k:Float): Float {
asegura: {res=1 <-> k == 0}
aegura: {res=0 <-> k /= 0}
}
 -}

 {-
 f2 devuelve 15 si la entrada es 1 y f 1 devuelve -15 si la entrada es -1

 problemaf2 (n:Float): Float {
    requiere: {n == 1 || n == -1}
    aegura: {res = 15 <-> n== 1}
    asegura: {res = -15 <-> n== -1}
 }
 -}

 {-
 f3 si n es menor o igual a 9 res = 7 o si n >= 3 res = 5

 problemaf3 (n:Float): Float {
 requiere: {True}
 asegura: {res = 7 <-> n <= 9}
 asegura: {res = 5 <-> n >= 3}
 }
 -}

 {- f4 es el resultado de sumar dos numeros x y y dividirlos por 2
 
 problemaf3 (x:Float, y:Float): Float {
    requiere: {True}
    asegura: {res= a+ b / 2}
 }
  -}

{- f5 es el resultadod de sumar la tupla (x, y) y dividirla por 2 

problemaf5 (x, y : (Float, FLoat)): Float {
requiere: {True}
asegura: {res = al promedio entre los elementos de la tupla (x,y)}

}
-}

{- f6 
problemaf6 (x:Float, y: Int): Bool  {
    reuqiere: {True}
    asegura: {res= True <-> la parte entera de x es y}
}

  -}



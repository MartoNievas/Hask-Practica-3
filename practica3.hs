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
    | x elem xs = filterNonRepeated (filter (/= x) xs) -- If x is repeated, filter it out
    | otherwise = x : filterNonRepeated xs             -- Otherwise, keep it

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z = sum . filterNonRepeated $ [x, y, z]

-- H
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = x mod y == 0

-- I
digitoUnidades :: Int -> Int 
digitoUnidades x = abs (x mod 10) 

-- J
digitoDecenas :: Int -> Int 
digitoDecenas x = digitoUnidades(x div 10)

{- EJERCICIO 4  -}
-- B
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor x y = fst x < fst y && snd x < snd y
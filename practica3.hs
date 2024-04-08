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
module Uno where 

--Funcion f que le meto un entero y devuelve otro 

f :: Int -> Int 
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16


g :: Int -> Int 
g n | n == 8 = 16 
    | n == 16 = 4 
    | n == 131 = 1
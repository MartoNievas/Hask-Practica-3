relacionesValidas :: [(String,String)] -> Bool 
relacionesValidas [] = True     
--relacionesValidas ((x1,x2):xs) = x1 /= x2  esta linea esta de mas 
relacionesValidas ((x1,x2):xs) | x1 == x2 || pertence (x1,x2) xs || pertence (x2,x1) xs = False 
                               | otherwise = relacionesValidas xs  

pertence :: (String,String) -> [(String,String)] -> Bool 
pertence _ [] = False 
pertence a (x:xs) = a == x || pertence a xs 

-- segunda parte 

personas :: [(String, String)] -> [String]
personas [] = ["nadie"] 
personas ((x1,x2):xs) = quitaRepetidos ( listaDePersonas ((x1,x2):xs))

listaDePersonas :: [(String,String)] -> [String]
listaDePersonas [(x1,x2)] = [x1 , x2]
listaDePersonas ((x1,x2):xs) = x1 : x2 : listaDePersonas xs 

quitaRepetidos :: [String] -> [String]
quitaRepetidos [] = []
quitaRepetidos (x:xs) = x : quitaRepetidos (quitarTodos x xs)


quitarTodos :: String -> [String] -> [String]
quitarTodos _ [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs 
                     | otherwise = x : quitarTodos e xs  

-- 


-- personas [("ana", "pedro"), ("ana", "carlos")]
-- = personas ("ana", "pedro"):[("ana", "carlos")]
-- = quitaRepetidos ( listaDePersonas (("ana", "pedro"):[("ana", "carlos")]))
-- = quitaRepetidos ["ana", "pedro", "ana", "carlos"]
-- = quitaRepetidos "ana":["pedro", "ana", "carlos"]
-- = "ana" : quitaRepetidos (quitarTodos "ana" ("ana":["pedro", "ana", "carlos"]))


                                       
-- http://10.41.105.62:8000/init_examen



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





--  Ejercicio 1
{-
 Ejercicio 1
Para empezar a diseñar la novedosa y rupturista red social Y el famoso Elio Mark nos ha pedido que desarrollemos algunas funciones básicas, que tendrán como objetido representar algunas relaciones e interacciones entre los usuarios. Para esto nos envió las siguientes especificaciones en lenguaje semiformal y nos pidió que hagamos el desarrollo enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que se ven en Introducción a la Programación de Exactas-UBA.

problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
  requiere: {True}
  asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
}
1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.

problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res no tiene elementos repetidos}
  asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
}

problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
}

problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
  requiere: {relaciones no vacía}
  requiere: {relacionesValidas(relaciones)}
  asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
}
-}



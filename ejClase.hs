

--Cuenta los elemntos de una lista

cuenta :: [tipo] -> Int
cuenta [] = 0
cuenta (_:resto) = 1 + cuenta resto

-- un tipo de dato creado
data Booleano = Falso | Verdadero deriving Show

-- arbol --
-- data Arbol e =
--      Nodo (Arbol e) e (Arbol e)
--     | ArbolVacio


data AB e = A (AB e) e (AB e) | AV derving Show

data LA e = L [LA e] | E e deriving Show

arbol = A (A (A AV 3 AV) 2 (A AV 4 AV) 1 (A AV 5 AV)

-- cuenta los nodos del arbol

carbol :: AB a -> Int
carbol AV = 0
carbol (A i _ d) = 1 + carbol i + carbol d 

-- car en haskell
lista = L [ E 1, L [E 2, E 3], E 4]
car :: LA e -> LA e
car (L (p: _)) = P


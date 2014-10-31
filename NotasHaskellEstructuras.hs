-- fromIntegral num -- para castear entero a un double porque haskell es fuertemente tipado
-- _ variable anónima
data Booleano = Falso | Verdadero deriving Show -- cada uno de ellos es un constructor de datos
--nombre de tipo de dato tiene que ser con mayuscula
-- tipo de datos es un derivado de clase show y la va usar para desplegar datos
-- parecido a enum en c y c++

-- tipos recursivos, se pueden cambiar a INT si quieres que todos sean int o e y son de cualquier tipo
data Arbol e =
  Nodo (Arbol e) e (Arbol e)| ArbolVacio

izq :: Arbol a -> Arbol a
izq (Nodo l n r) = l

der :: Arbol a -> Arbol a
der (Nodo l n r) = r

--recorrido:: Arbol a -> [a]
--recorrido ArbolVacio = []
--recorrido [Nodo l n r] =
  --(recorrido l)++[n]++(recorrido r)

data AB e = A (AB e) e (AB e) | AV deriving Show

data LA e = L [LA e] | E e deriving Show

arbolito  = A (A (A AV 3 AV) 2 (A AV 4 AV)) 1 (A AV 5 AV)

-- este programa : cuenta los nodos del arbol
cuentaNodos:: AB a -> Int
cuentaNodos AV = 0
cuentaNodos (A i _ d) = 1 + cuentaNodos i + cuentaNodos d

lista = L [E 1, L [ E 2, E 3], E 4]

car :: LA e ->  LA e
car (L (p:_))= p -- car definido nada mas para L

cdr :: LA e -> LA e
cdr (L (_:r)) = L r -- si pones r no jala, le pones L para que sea una lista anidada

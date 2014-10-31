--Andres G. Cavazos A01195067
--Gustavo Ferrufino A00812572

--1.	  Programar la función recursiva promedio en Haskell que obtenga la matrícula y 
-- promedio de un grupo de alumnos con registros en formato: 
-- (matrícula (nombre) (calificaciones)). El número de alumnos y calificaciones puede variar.


promedio :: [(Integer,[Char],[Integer])] -> [(Integer,Integer)]
promedio [] = []
promedio ((mat,nombre,calificaciones):resto) = 
	 (une [(prom (mat,nombre,calificaciones))]  (promedio resto))
	      
une :: [a] -> [a] -> [a]
une xs [] = xs
une [] ys = ys
une (x:xs) (y:ys) = x : y : une xs ys

prom :: (Integer,[Char],[Integer]) -> (Integer,Integer)
prom (matricula,_,calificaciones) = 
     (matricula, (promedioAux calificaciones) `div` (cuantos calificaciones))

promedioAux :: [Integer] -> Integer
promedioAux [] = 0
promedioAux (x:xs) = x + (promedioAux xs)

cuantos :: [Integer] -> Integer
cuantos [] = 0
cuantos (x:xs) = 1 + (cuantos xs)

--2.	Programar la función recursiva hojas en Haskell que obtenga una lista con los valores 
-- de los nodos hoja de un árbol binario con tipo de datos:

data AB e = N (AB e) e (AB e) | AV deriving Show

hojas  :: AB a -> [a]
hojas  AV = []
hojas  (N AV e AV) = [e]
hojas  (N l _ AV) = (hojas  l)
hojas  (N AV _ r) = (hojas  r)
hojas  (N l _ r) = (une (hojas  l) (hojas  r))

--3.   Programar la función recursiva binariza en Haskell que binariza una lista anidada de 
-- números enteros sustituyendo pares con 1 e impares con 0, donde la lista tiene el tipo de 
-- datos:
data LA e = L [LA e] | E e deriving Show

binariza :: LA Integer -> LA Integer 
binariza (L[]) =  L[]
binariza (E num) =
     if (even num) then
        E 1
        else
            E 0
binariza (L (x:resto)) = map binariza (L x)



	 




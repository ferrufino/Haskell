
-- Andres G. Cavazos A01195067 
-- Gustavo Ferrufino A00812572

--1. Implementar la función recursiva distintos que liste los elementos distintos que
--pertenecen a dos listas. Asumir que los elementos no se repiten dentro de la misma
--lista.
--Probar con:
-- distintos [‘a’,‘b’,‘c’] [‘d’,‘f’,‘a’] => [‘b’,‘c’,‘d’,‘f’]
-- distintos [1,2,3] [2,3,1,2] => []

distintos ::(Eq a) => [a] -> [a] -> [a]
distintos x y
	| (null x) = nub [ x | x <- (y)]
	| (null y) = nub [ x | x <- (x)]
	| (find (== (head x)) y) == Nothing = (head x) : (distintos (tail x) y)
	| otherwise = (distintos (tail x) (delete (head x) y))
	
--2. Implementar la función recursiva multiplica que obtenga una lista de 1’s que
--represente el resultado en unario de multiplicar dos enteros no negativos en decimal.
--Probar con:
-- multiplica 2 3 => [1,1,1,1,1,1]
-- multiplica 3 0 => []

multiplica :: Int -> Int -> [Int]
multiplica a b = multiplicaAux (a*b)

multiplicaAux :: Int ->[Int]
multiplicaAux a =
	if(a == 0) then
		[]
	else
		1 : multiplicaAux (a-1)


--3. Implementar la función recursiva bolos que genere el patrón de acomodo común para
--N bolos. La última línea de bolos puede quedar incompleta!
--Probar con:
-- bolos 0 => []
-- bolos 4 => [[1],[2,3],[4]]
-- bolos 10 => [[1],[2,3],[4,5,6],[7,8,9,10]]

bolos :: Int -> [[Int]]
bolos a = 
	if(a < 1) then
		[]
	else
		bolosAux a 1 1
		
bolosAux :: Int -> Int -> Int -> [[Int]]
bolosAux cuantos actual cantXbloque= 
	if(cuantos < 1) then
		[]
	else 
		construye cuantos actual cantXbloque : bolosAux (cuantos - cantXbloque) (actual + cantXbloque) (cantXbloque +1) 

construye :: Int -> Int -> Int ->[Int]
construye cuantos numero bloque =
	if ( bloque == 0 || cuantos < 1) then
		[]
	else
		numero:construye (cuantos-1) (numero+1)  (bloque-1)

-- Arbol binario
data AB t = A (AB t) t (AB t) | V deriving Show

-- Ejemplo
ab = A (A (A V 2 V) 5 (A V 7 V)) 8 (A V 9 (A (A V 11 V) 15 V)) 		

--4. Implementar la función obten-mayores en Scheme que dados un árbol binario y un
--valor como argumentos, cree una lista con los valores de los nodos que contengan
--valores mayores que el valor dado como argumento. Los valores en la lista resultante
--pueden, o no, estar ordenados.
--Probar con:
-- obten_mayores ab 2 => [8,5,7,9,15,11]
-- obten_mayores ab 8 => [9,15,11]
-- obten_mayores ab 20 => []

obten_mayores :: AB Integer -> Integer -> [Integer]
obten_mayores V _= []
obten_mayores (A l n V) num = 
	if(n > num) then
		n : (obten_mayores l num)
	else
		(obten_mayores l num) 
obten_mayores (A V n r) num = 
	if(n > num) then
		n : (obten_mayores r num)
	else
		(obten_mayores r num)
obten_mayores (A l n r) num = 
	if(n > num) then
		n : (obten_mayores l num) ++ (obten_mayores r num)
	else
		(obten_mayores l num) ++ (obten_mayores r num)

		
--5. Implementar la función internos en Scheme que dado un árbol binario regrese una
--lista con los valores que se encuentran en los nodos internos del árbol.
--Probar con:
-- internos V => []
-- internos ab => [8,5,9,15]
-- internos (A (A (A V 3 V) 2 V) 1 (A (A V 5 V) 4 V)) => [1,2,4]

internos :: AB Integer -> [Integer]
internos V = []
internos (A V n V) = []
internos (A l n V) = n : (internos l)
internos (A V n r) = n : (internos r)
internos (A l n r) = [n] ++ (internos l) ++ (internos r)

--6. Implementar la función recursiva g_distintos que utilizando “guardias” liste los
--elementos distintos que pertenecen a dos listas. Asumir que los elementos no se
--repiten dentro de la misma lista.
--Probar con:
-- g_distintos [‘a’,‘b’,‘c’] [‘d’,‘f’,‘a’] => [‘b’,‘c’,‘d’,‘f’]
-- g_distintos [1,2,3] [2,3,1] => []

--Problema 6:
g_distintos ::(Eq a) => [a] -> [a] -> [a]
g_distintos a b 
	| (null a) = [ x | x <- (b)]
	| (null b) = [ x | x <- (a)]
	| otherwise = foldl (flip delete) a b

--7. Implementar la función no-recursiva c_tabla en Haskell que utilizando “comprensión
--de listas” obtenga la tabla de multiplicar especificada. Los elementos de la tabla deben
--aparecer en tuplas.
--Probar con:
-- c_tabla 1 => [((1,1),1),((1,2),2), …, ((1,10),10)]
-- c_tabla 8 => [((8,1),8),((8,2),16), …, ((8,10),80)]



--8. Implementar la función no-recursiva f_prodpar en Haskell que utilizando la FOS
--(funciones de orden superior) cree una lista con los productos de los elementos de las
--listas de tamaño impar.
--Probar con:
-- f_prodpar [[1,2,3],[4,5],[6,7]] => [6]
-- f_prodpar [[1],[1,2],[1,2,3],[4,3,2]] => [1,6,24]

f_prodpar :: [[Int]] -> [Int]
f_prodpar (x) = map product (filter (odd . length) x)

























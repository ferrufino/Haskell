-- Andres Cavazos
--Gustavo Ferrufino
--1.	Programar la función cuenta_signo en Haskell usando guardias que a partir de una lista de
--		enteros que recibe como argumento, cuente el total de negativos y positivos y los regrese 
--		en el par (#negativos, #positivos). El cero no se cuenta.

cuenta_signo:: [Int] -> (Int, Int)
cuenta_signo l
  | l == [] = (0,0)
  | otherwise = (length [x | x<-l, x<0] , length [x | x<-l, x>0] )
  
--Probar con:
--cuenta_signo [-3,2,0,1,-1]	=> (2,2)
--cuenta_signo [0,1,2,3]		=> (0,3)

--2.	Programar la función separa en Haskell usando comprensión de listas que separe una lista de 
--		enteros en dos listas que contengan los valores menores que N en una y los mayores e iguales
--		a N en la otra, dentro de un par.

separa :: Integer -> [Integer] -> ([Integer], [Integer])
separa e l = (separa1aux e 1 l, separa1aux e 0 l)

separa1aux:: Integer -> Integer -> [Integer] -> [Integer]
separa1aux a b [] = []
separa1aux a b lista =
 if b == 1 then
 (\x -> if head x < a then (head x) : separa1aux a b (tail x) else separa1aux a b (tail x)) lista
 else
 (\x -> if head x >= a then (head x) : separa1aux a b (tail x) else separa1aux a b (tail x)) lista
	
-- Guardias
separa2 :: Integer -> [Integer] -> ([Integer], [Integer])
separa2 e l = (separa2aux e 1 l, separa2aux e 0 l)

separa2aux :: Integer -> Integer -> [Integer] -> [Integer]
separa2aux a b [] = []
separa2aux a b (x:xs)
 | b == 1 = if x < a then x : separa2aux a b xs else separa2aux a b xs
 | b == 0 = if x >= a then x : separa2aux a b xs else separa2aux a b xs


-- Comprension de listas
separa3 :: Integer -> [Integer] -> ([Integer], [Integer])
separa3 e l =
        (la, lb)
        where
        la = [y | y <- l, y < e]
        lb = [y | y <- l, y >= e]
		
--Probar con:
--separa 0 [-3,2,0,1,-1]		=> ([-3,-1],[2,0,1])
--separa 2 [0,1,2,3]		=> ([0,1],[2,3])

--3.	Programar la función fexp en Haskell usando listas infinitas que calcule la función 
--		exponencial a partir de N términos de la siguiente serie infinita:
 
--Probar con:
--fexp 4.0 1 		=> 1
--fexp 4.0 2 		=> 5.0 
--fexp 4.0 7 		=> 48.5555555555556
--fexp 4.0 28 	=> 54.598150033144

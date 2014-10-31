-- puntos extra 1 ---

-- 1 --
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- 2 --

mayor :: [Integer] -> Integer
mayor [] = 0
mayor [x] = x
mayor (x: xs) =
      if x > mayor xs
      	 then x
	 else mayor xs
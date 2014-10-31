--Evaluación Perezosa

positivos::[Int] -> [Int]
positivos l = [x | x <- l, x > 0]

qsort::[Int] -> [Int]
qsort [] = []
qsort (x:xs) =
  qsort lt ++ [x] ++ qsort ge
  where
    lt = [y | y <- xs, y < x]
    ge = [y | y <- xs, y >= x]

-- estructuras infinitas

cuenta_signo:: [Int] -> (Int, Int)
cuenta_signo [] = (0, 0)
cuenta_signo (x:xs)
  | positive x =
  | otherwise = 

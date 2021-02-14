busquedaLista :: Eq a => [a] -> a -> Bool
busquedaLista [] e = False
busquedaLista (x:xs) e = if x == e
			then True
			else busquedaLista xs e

factorial :: Int  -> Int
factorial 0 = 1
factorial n = n*factorial(n-1)

funcionM2 :: [Int] -> [Int]
funcionM2 [] = []
funcionM2 (x:xs) = (2*x:funcionM2 xs)

funcionMM2 :: [Int] -> [Int]
funcionMM2 xs = [2*x | x <- xs]
productoCartesiano :: [Int] -> [Int] -> [(Int,Int)]
productoCartesiano xs ys = [(x,y)|x<- xs, y <- ys]











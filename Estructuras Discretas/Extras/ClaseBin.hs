binarioA :: Int -> String

binarioA 0 = "0"
binarioA 1 = "1"
binarioA n = binarioA (mod n 2) ++binarioA (div n 2)

reversa ::[a]-> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x] 


binarioC :: Int -> String
binarioC n = if n>=0 
			 then ('0':reversa(binarioA n))
			 else ('1':reversa(binarioA(-n)))

			 --
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = longitud xs +1

decimalA:: String -> Int
decimalA [] = 0
decimalA(x:xs) = if x == '0'
			     then decimalA xs
			     else 2^(longitud xs)+
			     (decimalA xs) 

decimalC :: String -> Int
decimalC (x:xs) = if x == '0'
				  then decimalA xs
				  else (	-1)*(decimalA xs)


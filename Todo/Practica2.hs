--Bonilla Ruiz Roberto Adrián
--Ejercicios

conjuncion :: Bool -> Bool -> Bool 
conjuncion True True = True
conjuncion p q = False 

disyuncion :: Bool -> Bool -> Bool 
disyuncion False False = False 
disyuncion p q = True 

implicacion :: Bool -> Bool -> Bool
implicacion True False = False
implicacion p q = True

dobleImplica :: Bool -> Bool -> Bool
dobleImplica     True True = True
dobleImplica     False False = True
dobleImplica p q = False 

--Listas

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = longitud xs +1

sumaNumeros :: Num a => [a] -> a
sumaNumeros [] = 0
sumaNumeros (x:xs) = sumaNumeros xs + x

maximo :: Ord  a => [a] -> a
maximo [x] = x
maximo (x:xs) = if x > maximo xs
					then x
					else maximo xs 

indiceDe :: Int ->[a] -> a
indiceDe 0 (x:xs) = x
indiceDe i (x:xs) = if i<0 || i>longitud (x:xs) 
					then error "indice incompatible"
					else indiceDe(i-1)xs


insertarElemento :: a -> [a] -> Bool -> [a]
insertarElemento e xs True = (e:xs)
insertarElemento e (x:xs) False =(x:xs)++[e]					


reversa ::[a]-> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x] 

esPalindromo :: Eq a => [a] -> Bool
esPalindromo [] = True
esPalindromo xs = xs == reversa xs


busquedaLista :: Eq a => [a] -> a -> Bool
busquedaLista [] e = False
busquedaLista (x:xs) e = if x == e
			then True
			else busquedaLista xs e

aConjunto :: Eq a => [a] -> [a]
aConjunto [] = [] 
aConjunto (x:xs) = if  busquedaLista xs x
				   then aConjunto xs
				   else [x] ++ aConjunto xs 

union :: Eq a=> [a] -> [a]	-> [a]
union xs xy = aConjunto (xs ++ xy)

interseccion:: Eq a=> [a] -> [a] -> [a]
interseccion [] xs = []
interseccion xs [] = []
interseccion (x:xs) (e:ey) = if busquedaLista (e:ey) x 
							 then aConjunto ([x] ++ interseccion xs (e:ey))
							 else interseccion xs (e:ey)


productoCruz :: [a] -> [b] -> [(a,b)]
productoCruz [] [] = []
productoCruz xs [] = []
productoCruz xs ys = [(x,y) | x<-xs, y<-ys]

diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] xs = []
diferencia (x:xs) ys = if busquedaLista ys x
				  	   then diferencia xs ys 
				  	   else (x:(diferencia xs ys))


diferenciaSimetrica:: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = union (diferencia xs ys)
								  (diferencia ys xs) 


--Puntos Extras
divisible:: Int -> Int -> Bool
divisible x y = (mod x y) == 0 

divisores:: Int -> [Int]
divisores x = [ y | y <- [1..x-1],divisible x y]



conjuntoPotencia:: Eq a => [a] -> [(a,a)]
conjuntoPotencia  xs = [(x,x) | x<-xs]
			 







							 

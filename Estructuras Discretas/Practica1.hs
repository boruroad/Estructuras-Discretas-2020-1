--Bonilla Ruiz Roberto Adrian

suma:: Int->Int->Int
suma a b = a+b

resta:: Int->Int->Int
resta a b = a-b

multiplicacion:: Float->Float->Float
multiplicacion a b = a*b

division:: Float->Float->Float
division a b = a/b

comparador :: Float->Float->Int
comparador a b = if (a==b) 
		 then 0
		 else if a>b
		 then 1
		 else -1

potencia:: Int->Int->Int
potencia a b = a^b

maximo:: Float->Float->Float->Float
maximo a b c =
	     if a>b
	     then if a>c
	     then a
	     else c
	     else if b>c
	     then b
	     else c

dist:: Float->Float->Float->Float->Float
dist x1 y1 x2 y2 = sqrt((x2-x1)^2+(y2-y1)^2)

hipotenusa:: Float->Float->Float
hipotenusa a b = sqrt((a^2)+(b^2)) 

pendiente:: Float->Float->Float->Float->Float
pendiente y1 y2 x1 x2 = (y2-y1)/(x2-x1)

raicesCuadraticas:: Float->Float->Float->(Float,Float)
raicesCuadraticas a b c =  ((-b-d)/e,(-b+d)/e)
                where d = sqrt(b*b-4.0*a*c)
                      e = 2.0*a

anguloApotema :: Double->Double
anguloApotema n = (2*3.14)/n

apotema :: Double->Double->Double
apotema l n = l/(2*tan((anguloApotema n)/2))

areaBase :: Double->Double->Double
areaBase l n = ((l*n)*(apotema l n))/2 

volumen :: Double->Double->Double->Double
volumen l n h = (areaBase l n)*(h)/3
	




 






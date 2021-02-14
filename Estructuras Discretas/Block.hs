

						else (equivalencia xs) :&: (equivalencia ys)
interp (xs :|: ys) = 
						  if xs == ys
						  then equivalencia xs
						  else (equivalencia xs) :|: (equivalencia ys)

interp (xs :=>: ys) = negar(equivalencia xs) :|: (equivalencia ys)

interp (xs :<=>: ys) = equivalencia(xs :=>: ys) :&: equivalencia(ys :=>: xs)


  (Neg(equivalencia xs) :|: (equivalencia ys)) :&: (Neg(equivalencia ys) :|: (equivalencia xs))
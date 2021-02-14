data Natural = Cero | Suc Natural deriving (Show, Eq, Ord)

suma :: Natural -> Natural -> Natural 
suma n Cero = n
suma Cero n = n 
suma (Suc n) m = Suc (suma n m)

multiplicacion :: Natural -> Natural -> Natural
multiplicacion Cero n = Cero
multiplicacion n Cero = Cero 
multiplicacion (Suc n) m = (multiplicacion n m )

potencia:: Natural -> Natural -> Natural
potencia Cero = error "Indefinido"
potencia Cero n = Cero
potencia n Cero = Suc Cero
potencia m (Suc n) = multiplicacion m (potencia m n)
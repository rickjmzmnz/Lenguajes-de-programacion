
--Sintaxis concreta de EAB2
data EAB2 = V String 
	  | Vn Int 
	  | Vb Bool 
	  | Rt EAB2 EAB2 
	  | Mq EAB2 EAB2 
	  | If EAB2 EAB2 EAB2 
	  | IsZ EAB2 
	  | Let String EAB2 EAB2 deriving Show 

sust :: EAB2->String->EAB2->EAB2
sust e s r =  case e of
		V v -> if (s == v) then r else e
		Vn n -> Vn n
		Vb b -> Vb b
		Rt e1 e2 -> Rt (sust e1 s r) (sust e2 s r)
		Mq e1 e2 -> Mq (sust e1 s r) (sust e2 s r)
		If e1 e2 e3 -> If (sust e1 s r) (sust e2 s r) (sust e3 s r)
		IsZ e1 -> IsZ $ sust e1 s r
		l@(Let s1 e1 e2) -> if (elem s1 (fv r ++ [s]))
				then l
				else (Let s1 (sust e1 s r) (sust e2 s r)) 
				

fv :: EAB2->[String]
fv e = case e of
	V x -> [x]
	Vn _ -> []
	Vb _ -> []
	Rt e1 e2 -> fv e1 ++ fv e2
	Mq e1 e2 -> fv e1 ++ fv e2
	If e1 e2 e3 -> fv e1 ++ fv e2 ++ fv e3
	IsZ e1 -> fv e1
	Let x e1 e2 -> filter (/=x) (fv e1 ++ fv e2)

--Nos dice si una expresión es un valor.
esValor :: EAB2->Bool
esValor e = case e of
	    V _ -> False
	    Vn _ -> True
	    Vb _ -> True
	    Rt e1 e2 -> (esValor e1) && (esValor e2)
	    Mq e1 e2 -> (esValor e1) && (esValor e2)
	    If e1 e2 e3 -> (esValor e1) && (esValor e2) && (esValor e3)
	    IsZ e1 -> esValor e1
	    Let s e1 e2 -> (esValor e1) && (esValor e2)

--Nos dice si una expresión está bloqueada.
esBloq :: EAB2->Bool
esBloq e = case e of
	    V _ -> True
	    Vn _ -> True
	    Vb _ -> True
	    Rt e1 e2 -> (esBloq e1) || (esBloq e2)  
	    Mq e1 e2 -> (esBloq e1) || (esBloq e2)
	    If e1 e2 e3 -> (esBloq e1) || (esBloq e2) || (esBloq e3)
	    IsZ e1 -> esBloq e1
	    Let s e1 e2 -> (esBloq e1) || (esBloq e2)

--Realiza sólo un paso de evaluación aplicando una regla de transición.
eval1 :: EAB2->EAB2
eval1 e = case e of
	   Vn n -> Vn n
	   Vb b -> Vb b
	   Rt (Vn n) (Vn m) -> if(n>=m) 
				then (Vn (n-m))
				else Vn 0
	   Rt (Vn n) e2 -> Rt (Vn n) (eval1 e2)
	   Rt e1 e2 -> Rt (eval1 e1) e2
	   Mq (Vn n) (Vn m) -> if(n<m)
				then (Vb True)
				else (Vb False)
	   Mq (Vn n) e2 -> Mq (Vn n) (eval1 e2)
	   Mq e1 e2 -> Mq (eval1 e1) e2
	   If (Vb True) e1 e2 -> e1
	   If (Vb False) e1 e2 -> e2
	   If e1 e2 e3 -> If (eval1 e1) e2 e3
	   IsZ (Vn n) -> if(n == 0)
			  then (Vb True)
			  else (Vb False)
	   IsZ e -> IsZ (eval1 e)
	   Let s (Vn n) e -> sust e s (Vn n)
	   Let s (Vb b) e -> sust e s (Vb b)
	   Let s e1 e2 -> Let s (eval1 e1) e2

--Realiza la evaluación hasta llegar a una expresión bloqueada.
evals :: EAB2->EAB2
evals (Vn n) = Vn n
evals (Vb b) = Vb b
evals e = evals(eval1 e)	

{-Pruebas-}
--Debe regresar Vb True
prueba1 = evals $ Let "x" (Rt (Vn 2) (Vn 3)) (If (IsZ (V "x")) (Mq (Vn 5) (Vn 6)) (IsZ (Vn 4)))
--Debe regresar Vn 3
prueba2 = evals $ If (IsZ (Rt (Vn 4) (Vn 3))) (IsZ (V "x")) (Let "x" (Rt (Vn 7) (Vn 2)) (Rt (V "x") (Vn 2))) 
--Debe regresar Vn 5
prueba3 = evals $ Rt (If (Mq (Vn 3) (Vn 4)) (Rt (Vn 14) (Vn 2)) (IsZ (Vn 3))) (Let "x" (Rt (Vn 5) (Vn 2)) (Rt (Vn 10) (V "x")))  

 
--Sintaxis concreta de EABS
data EABS = Vs String 
	  | Vns Int 
	  | Vbs Bool 
	  | Rts EABS EABS 
	  | Mqs EABS EABS 
   	  | Ifs EABS EABS EABS 
	  | IsZs EABS 
	  | Lets String EABS EABS
	  | S String
	  | Concat EABS EABS
	  | Len EABS
	  | IsNil EABS deriving Show

--Semántica estática de EABS
data Tipo = TNat | TBool | TString deriving Eq

type Ctx = [(String, Tipo)]

--Realiza la verificación de tipos de una expresión EABS.
vt :: Ctx->EABS->Tipo
vt est e = case e of
	  (Vs x) -> var est (Vs x)
 	  (Vns n) -> TNat
 	  (Vbs b) -> TBool
 	  (Rts e1 e2) -> if ((vt est e1 == TNat) && (vt est e2 == TNat))
		         then TNat
		         else error "tipos incorrectos"
 	  (Mqs e1 e2) -> if ((vt est e1 == TNat) && (vt est e2 == TNat))
 		         then TBool
		         else error "tipos incorrectos"
	  (Ifs e1 e2 e3) -> if ((vt est e1 == TBool) && (vt est e2 == vt est e3))
			    then vt est e2
			    else error "tipos incorrectos"
 	  (IsZs e) -> if (vt est e == TNat)
		      then TBool
		      else error "tipo incorrecto"
	  (Lets s e1 e2) -> vt ([(s,(vt est e1))]++ est) e2
 	  (S _) -> TString
 	  (Concat s1 s2) -> if ((vt est s1 == TString) && (vt est s2 == TString))
 			    then TString
			    else error "tipos incorrectos"
 	  (Len s) -> if (vt est s == TString)
		     then TNat
		     else error "tipo incorrecto"
 	  (IsNil s) -> if (vt est s == TString)
		       then TBool
		       else error "tipo incorrecto"

var :: Ctx->EABS->Tipo
var [] _ = error "no se encuentra la variable"
var ((s,t):ts) (Vs x) = if (x==s) 
			then t 
			else var ts (Vs x)
 
--Semántica dinámica.

--La función sustEABS e x r debe comportarse así: e[x:=r]
sustEABS :: EABS->String->EABS->EABS
sustEABS e x r = case e of
		Vs s -> if (s == x) then r else e
		Vns n -> Vns n
		Vbs b -> Vbs b
		Rts e1 e2 -> Rts (sustEABS e1 x r) (sustEABS e2 x r)
		Mqs e1 e2 -> Mqs (sustEABS e1 x r) (sustEABS e2 x r)
		Ifs e1 e2 e3 -> Ifs (sustEABS e1 x r) (sustEABS e2 x r) (sustEABS e3 x r)
		IsZs e -> IsZs (sustEABS e x r)
		l@(Lets s e1 e2) -> if (elem s (fv r ++ [s]))
				then l
				else (Lets s (sustEABS e1 s r) (sustEABS e2 s r))
		S s -> S s
		Concat e1 e2 -> Concat (sustEABS e1 x r) (sustEABS e2 x r)
		Len e -> Len (sustEABS e x r)
		IsNil e -> IsNil (sustEABS e x r)

fv :: EABS->[String]
fv e = case e of
	Vs x -> [x]
	Vns _ -> []
	Vbs _ -> []
	Rts e1 e2 -> fv e1 ++ fv e2
	Mqs e1 e2 -> fv e1 ++ fv e2
	Ifs e1 e2 e3 -> fv e1 ++ fv e2 ++ fv e3
	IsZs e1 -> fv e1
	Lets x e1 e2 -> filter (/=x) (fv e1 ++ fv e2)
	S s -> []
	Concat e1 e2 -> fv e1 ++ fv e2
	Len e -> fv e
	IsNil e -> fv e	

--Nos dice si una expresión es un valor.
esValorEABS :: EABS->Bool
esValorEABS e = case e of
		Vs _ -> False
		Vns _ -> True
		Vbs _ -> True
		Rts e1 e2 -> (esValorEABS e1) && (esValorEABS e2)
		Mqs e1 e2 -> (esValorEABS e1) && (esValorEABS e2)
		Ifs e1 e2 e3 -> (esValorEABS e1) && (esValorEABS e2) && (esValorEABS e3)
		IsZs e -> (esValorEABS e)
		Lets x e1 e2 -> (esValorEABS e1) && (esValorEABS e2)
		S _ -> False
		Concat e1 e2 -> (esValorEABS e1) && (esValorEABS e2)
		Len e -> (esValorEABS e)
		IsNil e -> (esValorEABS e)

--Realiza sólo un paso de evaluación aplicando una regla de transición.
eval1EABS :: EABS->EABS
eval1EABS e = case e of
	      Vns n -> Vns n
	      Vbs b -> Vbs b
	      Rts (Vns n) (Vns m) -> if(n>=m) 
				     then (Vns (n-m))
				     else Vns 0
	      Rts (Vns n) e2 -> if(vt [] e2 == TNat)
				then Rts (Vns n) (eval1EABS e2)
				else error "tipo incorrecto"
	      Rts e1 e2 -> if(vt [] e1 == TNat)
			   then Rts (eval1EABS e1) e2
			   else error "tipo incorrecto"
	      Mqs (Vns n) (Vns m) -> if(n<m)
				     then (Vbs True)
				     else (Vbs False)
	      Mqs (Vns n) e2 -> if(vt [] e2 == TNat)
				then Mqs (Vns n) (eval1EABS e2)
				else error "tipo incorrecto"
	      Mqs e1 e2 -> if(vt [] e1 == TNat)
			   then Mqs (eval1EABS e1) e2
			   else error "tipo incorrecto"
	      Ifs (Vbs True) e1 e2 -> e1
	      Ifs (Vbs False) e1 e2 -> e2
	      Ifs e1 e2 e3 -> if(vt [] e1 == TBool && (vt [] e2 == vt [] e3))
			      then Ifs (eval1EABS e1) e2 e3
			      else error "tipos incorrecto"
	      IsZs (Vns n) -> if(n == 0)
			      then (Vbs True)
			      else (Vbs False)
	      IsZs e -> if(vt [] e == TNat)
			then IsZs (eval1EABS e)
			else error "tipo incorrecto"
	      Lets s (Vns n) e -> sustEABS e s (Vns n)
	      Lets s (Vbs b) e -> sustEABS e s (Vbs b)
	      Lets s (S x) e -> sustEABS e s (S x)
	      Lets s e1 e2 -> Lets s (eval1EABS e1) e2
	      S s -> S s
	      Concat (S x) (S y) -> S (x++y)
	      Concat (S x) e -> if(vt [] e == TString)
				then Concat (S x) (eval1EABS e)
				else error "tipo incorrecto"
	      Concat e1 e2 -> if (vt [] e1 == TString)
			      then Concat (eval1EABS e1) e2
			      else error "tipo incorrecto"
	      Len (S s) -> if (s == "")
			   then error "cadena vacia"
			   else Vns (length s)
	      Len e -> if (vt [] e == TString)
		       then Len (eval1EABS e)
		       else error "tipo incorrecto"
  	      IsNil (S s) -> if (s == "")
			     then Vbs True
			     else Vbs False
	      IsNil e -> if (vt [] e == TString)
			 then IsNil (eval1EABS e)
			 else error "tipo incorrecto"

--Evalúa una expresión. Si hay errores de tipos o errores de ejecución,
--deben mostrarse mensajes que informen acerca de ese error.
evalEABS :: EABS->EABS
evalEABS (Vns n) = Vns n
evalEABS (Vbs b) = Vbs b
evalEABS (S s) = S s
evalEABS e = evalEABS(eval1EABS e)

{-Pruebas-}
--Debe regresar Vbs True
prueba1 = evalEABS $ Lets "x" (Rts (Vns 2) (Vns 3)) (Ifs (IsZs (Vs "x")) (Mqs (Vns 5) (Vns 6)) (IsZs (Vns 4)))
--Debe regresar Vns 5
prueba2 = evalEABS $ Ifs (IsZs (Rts (Vns 4) (Vns 6))) (Rts (Len (S "abcdefg")) (Len (S "xy"))) (Vns 3)
--Debe regresar Vbs False
prueba3 = evalEABS $ Lets "x" (Concat (S "ab") (S "cd")) (IsNil (Concat (Vs "x") (Vs "x")))
--Debe regresar "tipos incorrectos
prueba4 = evalEABS $ Ifs (IsZs (Rts (Vns 4) (Vns 3))) (IsZs (Vns 0)) (Lets "x" (Rts (Vns 7) (Vns 2)) (Rts (Vs "x") (Vns 2))) 
--Debe regresar "tipos incorrectos"
prueba5 = evalEABS $ Rts (Ifs (Mqs (Vns 3) (Vns 4)) (Rts (Vns 14) (Vns 2)) (IsZs (Vns 3))) (Lets "x" (Rts (Vns 5) (Vns 2)) (Rts (Vns 10) (Vs "x")))

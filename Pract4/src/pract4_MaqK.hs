{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-1 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: Susana Hahn Martín Lunas
		  Laboratorio: C. Moisés Vázquez Reyes-}



   {-MÁQUINA K-}

infixr :->

--Tipos
data Tipo = TNat | TBool | TError | Tipo :-> Tipo deriving (Show,Eq)

--Expresiones aritméticas-booleanas con funciones.   
data LamAB =  Var String |
              VNum Int   |
              VBool Bool |
              Suma LamAB LamAB | 
              Div LamAB LamAB |
              Ifte LamAB LamAB LamAB |
              Pred LamAB |
              Let String LamAB LamAB |
              Menor LamAB LamAB |
              Eq LamAB LamAB |
              Neg LamAB |
	      App LamAB LamAB |
              Lambda String Tipo LamAB |
              Fix String Tipo LamAB |
              Error |
              Catch LamAB LamAB deriving (Show,Eq)

--Pila de control
type Pila = [Marco]

--Marcos de operación
data Marco = MSumI () LamAB |
             MSumD LamAB () |
	     MDivI () LamAB |
	     MDivD LamAB () |
	     MIf () LamAB LamAB |
	     MPred () |
	     MLet String () LamAB |
	     MMenI () LamAB |
	     MMenD LamAB () |
	     MEqI () LamAB |
	     MEqD LamAB () |
	     MNeg () |
	     MAppI () LamAB |
	     MAppD LamAB () |
	     MCatchI () LamAB |
	     MCacthD LamAB () deriving (Show,Eq)

--Estados de la Máquina K
data EstadoMK = Ev (Pila,LamAB)   -- Ev(P,e) corresponde a evalua   P > e 
              | Dv (Pila,LamAB)   -- Dv(P,v) corresponde a devuelve P < v
              | Pg (Pila,LamAB)   -- Pg(P,e) corresponde a propaga  P << e
            deriving Show    


--SEMÁNTICA ESTÁTICA

sust :: LamAB -> String -> LamAB -> LamAB
sust e s r = case e of
			Var v -> if (v == s)
					then r
					else e
			VNum n -> VNum n
			VBool b -> VBool b
			Suma e1 e2 -> Suma (sust e1 s r) (sust e2 s r)
			Div e1 e2 -> Div (sust e1 s r) (sust e1 s r)
			Pred e -> Pred (sust e s r)
			Ifte e1 e2 e3 -> Ifte (sust e1 s r) (sust e2 s r) (sust e3 s r)
			l@(Let s1 e1 e2) -> if (elem s1 (fv r ++ [s]))
				then l
				else (Let s1 (sust e1 s r) (sust e2 s r))
			Menor e1 e2 -> Menor (sust e1 s r) (sust e2 s r)
			Eq e1 e2 -> Eq (sust e1 s r) (sust e2 s r)
			Neg e -> Neg (sust e s r)
			App e1 e2 -> App (sust e1 s r) (sust e2 s r)
			l@(Lambda s1 t e) -> if (elem s1 (fv r ++ [s]))
						then l
						else (Lambda s1 t (sust e s r))
			f@(Fix s1 t e) -> if (elem s1 (fv r ++ [s]))
						then f
						else (Fix s1 t (sust e s r))
			Error -> Error
			Catch e1 e2 -> Catch (sust e1 s r) (sust e2 s r)

fv :: LamAB->[String]
fv e = case e of
		Var v -> [v]
		VNum n -> []
		VBool b -> []
		Suma e1 e2 -> fv e1 ++ fv e2
		Div e1 e2 -> fv e1 ++ fv e2
		Ifte e1 e2 e3 -> fv e1 ++ fv e2 ++ fv e3
		Pred e -> fv e
		Let x e1 e2 -> filter (/=x) (fv e1 ++ fv e2)
		Menor e1 e2 -> fv e1 ++ fv e2
		Eq e1 e2 -> fv e1 ++ fv e2
		Neg e -> fv e
		App e1 e2 -> fv e1 ++ fv e2
		Lambda s t e -> filter (/=s) (fv e)
		Fix s t e -> filter (/=s) (fv e)
		Error -> []
		Catch e1 e2 -> fv e1 ++ fv e2

--Nos dice si un estado es final
esFinal :: EstadoMK->Bool
esFinal e = case e of
		Dv ([],Var v) -> True
		Dv ([],VNum n) -> True
		Dv ([],VBool b) -> True
		_ -> False

--Realiza un paso de evaluación en la máquina K
eval1 :: EstadoMK->EstadoMK
eval1 e = case e of
		Ev (p,Var v) -> Dv (p,Var v)
		Ev (p,VNum n) -> Dv (p,VNum n)
		Ev (p,VBool b) -> Dv (p,VBool b)
		Ev (p,(Lambda s t e)) -> Dv (p,(Lambda s t e))
		--Suma
		Ev (p,Suma e1 e2) -> Ev ((MSumI () e2):p,e1)
		Dv ((MSumI () e2):p,VNum n) -> Ev ((MSumD (VNum n) ()):p,e2)
		Dv ((MSumD (VNum n) ()):p,VNum m) -> Dv (p, VNum (n+m))
		--Div
		Ev (p,Div e1 e2) -> Ev ((MDivI () e2):p,e1)
		Dv ((MDivI () e2):p,VNum n) -> Ev ((MDivD (VNum n) ()):p,e2)
		Dv ((MDivD (VNum n) ()):p,VNum m) -> Dv (p, VNum (div n m))
		--If
		Ev (p,Ifte e1 e2 e3) -> Ev ((MIf () e2 e3):p,e1)
		Dv ((MIf () e2 e3):p,VBool True) -> Ev (p,e2)
		Dv ((MIf () e2 e3):p,VBool False) -> Ev (p,e3)
		--Pred
		Ev (p,Pred e1) -> Ev ((MPred ()):p,e1)
		Dv ((MPred ()):p,VNum n) -> Dv (p,VNum (pred n))
		--Let
		Ev (p,Let n e1 e2) -> Ev ((MLet n () e2):p,e1)
		Dv ((MLet n () e2):p,v) -> Ev (p,sust e2 n v)
		--Menor
		Ev (p,Menor e1 e2) -> Ev ((MMenI () e2):p,e1)
		Dv ((MMenI () e2):p,VNum n) -> Ev ((MMenD (VNum n) ()):p,e2)
		Dv ((MMenD (VNum n) ()):p,VNum m) -> Dv (p, VBool (n < m))
		--Eq
		Ev (p,Eq  e1 e2) -> Ev ((MEqI () e2):p,e1)
		Dv ((MEqI () e2):p,VNum n) -> Ev ((MEqD (VNum n) ()):p,e2)
		Dv ((MEqD (VNum n) ()):p,VNum m) -> Dv (p, VBool (n == m))
		--Neg
		Ev (p,Neg e1) -> Ev ((MNeg ()):p,e1)
		Dv ((MNeg ()):p,VBool b) -> Dv (p,VBool (not b))
		--App
		Ev (p,App e1 e2) -> Ev ((MAppI () e2:)p,e1)
		Dv ((MAppI () e2):p,v) -> Ev ((MAppD v ()):p,e2)
		Dv ((MAppD (Lambda s t e3) ()):p,v) -> Ev (p,sust e3 s v)
		--Fix
		Ev (p,Fix s t e) -> Ev (p,sust e s (Fix s t e))
		--error
		Ev (p,Error) -> Pg (p,Error)
		Pg (m:p,Error) -> case m of
				(MCatchI () e2) -> Ev (p,e2)
				_ -> Pg (p,Error) 
		--catch
		Ev (p,Catch e1 e2) -> Ev ((MCatchI () e2):p,e1)
		Dv ((MCatchI () e2):p,v) -> Dv (p,v)

--app(lambda(Bool,x.if not x then app((lambda(Nat,y.y)),2) else app(lambda(Nat,z.z+1),2)),iszero 2)

--Realiza una ejecución completa en la máquina K
evalK :: EstadoMK->EstadoMK
evalK e = if(esFinal e)
	  then e
	  else evalK(eval1 e)


{-Pruebas semántica dimámica-}
--Debe dar Dv ([],VBool True)
prueba1 = evalK $ Ev ([(MEqI () (VNum 5)),(MIf () (VBool True) (VBool False))],Div (VNum 25) (VNum 5))
--Debe dar Dv ([],VNum 2)
prueba2 = evalK $ Ev ([],App (Lambda ("x") (TBool) (Ifte (Eq (Var "x") (Var "x")) (App (Lambda ("y") (TNat) (Var "y")) (VNum 2)) (App (Lambda ("z") (TNat) (Suma (Var "z") (VNum 1))) (VNum 2)))) (Pred (VNum 2)))
--Debe dar Dv ([],VNum 7)
prueba3 = evalK $ Ev ([MIf () (Suma (Div (Pred (VNum 5)) (VNum 2)) (Suma (Pred (VNum 4)) (VNum 2))) (Pred(Pred(VNum 5)))],Neg (Eq (Let "x" (VNum 3) (Suma (Var "x") (Var "x"))) (VNum 4)))
--Debe dar Dv ([],VBool False)
prueba4 = evalK $ Ev ([(MDivI () (Pred (Suma (VNum 4) (VNum 4)))),(MSumI () (Pred (VNum 4))),(MCatchI () (Neg (Menor (VNum 3) (VNum 4))))],Error)
--Dv ([],VNum 7)
prueba5 = evalK $ Ev ([(MCatchI () (Pred (VNum 4))),(MSumI () (VNum 4))],Error)
--Dv ([],VBool True)
prueba6 = evalK $ Ev ([(MCatchI () (VNum 3)),(MEqI () (Suma (VNum 3) (VNum 4))),(MNeg ())],Error)

--SEMÁNTICA DINÁMICA

type Ctx = [(String,Tipo)] 

--Realiza la verificación de tipos
vt :: Ctx->LamAB->Tipo
vt ctx e = case e of
		Var x -> case ctx of
			([]) -> error $ "La variable "++show x++" no está en el contexto"
		  	((y,t):ctxV) -> if x==y 
						then t
						else vt (ctxV) e
		VNum _ -> TNat
		VBool _ -> TBool
		Suma e1 e2 -> let 
				t1 = vt ctx e1 
				t2 = vt ctx e2 in
					if (t1==TNat && t2==TNat) 
					then TNat 
					else (if (t1/=TNat) 
						then error $ "La expresión "++ show e1 ++" no es de tipo TNat" 
						else  error $ "La expresión "++ show e2 ++" no es de tipo TNat")
		Div e1 e2 -> let 
				t1 = vt ctx e1 
				t2 = vt ctx e2 in
					if (t1==TNat && t2==TNat) 
					then TNat 
					else (if (t1/=TNat) 
						then error $ "La expresión "++ show e1 ++" no es de tipo TNat" 
						else  error $ "La expresión "++ show e2 ++" no es de tipo TNat")
		
		Ifte e1 e2 e3 -> let
				   t1 = vt ctx e1
				   t2 = vt ctx e2
				   t3 = vt ctx e3 in
					if (t1 == TBool && t2 == t3) 
					then t2 
					else (if (t1/=TBool)
						then error $ "La expresión "++ show e1 ++" no es de tipo TBool"
						else error $ "Las expresiones "++ show e2 ++ " y "++ show e3 ++" no son del mismo tipo")
		Pred e -> let
			    t = vt ctx e in
				if (t == TNat)
				then TNat
				else error $ "La expresión "++ show e ++" no es de tipo TNat"
		Let s e1 e2 -> vt ([(s,(vt ctx e1))]++ ctx) e2		
		Menor e1 e2 -> let
				t1 = vt ctx e1
				t2 = vt ctx e2 in
					if (t1 == TNat && t2 == TNat)
					then TBool
					else (if (t1 /= TNat)
						then error $ "La expresión "++ show e1 ++" no es de tipo TNat"
						else error $ "La expresión "++ show e2 ++" no es de tipo TNat")
				
		Eq e1 e2 -> let
				t1 = vt ctx e1
				t2 = vt ctx e2 in
					if (t1 == TNat && t2 == TNat)
					then TBool
					else (if (t1 /= TNat)
						then error $ "La expresión "++ show e1 ++" no es de tipo TNat"
						else error $ "La expresión "++ show e2 ++" no es de tipo TNat")
		Neg e -> let
			    t = vt ctx e in
				if (t == TBool)
				then TBool
				else error $ "La expresión "++ show e ++" no es de tipo TBool"
		App e1 e2 -> let
				t2 = vt ctx e2 in
				let t1 = t2 :-> (vt ctx e1) in t2					 
		Lambda s t e -> t :-> (vt ([(s,t)]++ctx) e)				
		Fix s t e -> vt ([(s,t)]++ctx) e
		Error -> TError
		Catch e1 e2 -> let
				t1 = vt ctx e1
				t2 = vt ctx e2 in
					if (t1 == t2)
					then t1
					else error $ "Las expresiones "++ show e1 ++" y "++ show e2 ++" no son del mismo tipo"

{-Pruebas semántica estática-}
--Debe dar TBool
prueba7 = vt []  $ Ifte (Neg (Menor (Suma (VNum 4) (VNum 8)) (Div (VNum 2) (VNum 4)))) (Fix ("s") (TBool) (Neg (Var "s"))) (Eq (VNum 5) (VNum 3))
--Debe dar TNat :-> TNat
prueba8 = vt [("x",TNat),("y",TNat :-> TNat)] $ Catch (Var "y") (Lambda ("z") (TNat) (Div (Var "x") (Suma (Var "z") (Pred (VNum 3)))))
--Debe dar TError
prueba9 = vt [("e",TError),("n",TNat),("b",TNat)] $ Let "x" (Neg (Eq (Div (VNum 3) (Var "n")) (Var "b"))) (Ifte (Var "x") (Var "e") (Error))
--Debe dar TNat
prueba10 = vt [] $ App (Lambda ("x") (TNat) (Ifte (Eq (Var "x") (Var "x")) (App (Lambda ("y") (TNat) (Var "y")) (VNum 2)) (App (Lambda ("z") (TNat) (Suma (Var "z") (VNum 1))) (VNum 2)))) (Pred (VNum 2))
--Debe dar Las expresiones Var "n" y Var "b" no son del mismo tipo
prueba11 = vt [("n",TNat),("b",TBool)] $ Pred (Eq (Menor (Catch (Var "n") (Var "b")) (Neg (Var "b"))) (Suma (Pred (VNum 2)) (Var "n")))
--Debe dar Las expresiones Catch (Var "t") (Lambda "z" TBool Error) y Lambda "l" TError (Menor (Var "n") (VNum 4)) no son del mismo tipo
prueba12 = vt [("n",TNat),("t",TBool :-> TError)] $ Ifte (Fix ("s") (TNat) (Eq (Suma (Var "s") (Var "n")) (Div (Var "n") (Var "s")))) (Catch (Var "t") (Lambda ("z") (TBool) (Error))) (Lambda ("l") (TError) (Menor (Var "n") (VNum 4)))




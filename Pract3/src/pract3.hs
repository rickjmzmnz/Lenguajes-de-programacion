{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-1 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: Susana Hahn Martín Lunas
		  Laboratorio: C. Moisés Vázquez Reyes-}

data EAB = Var String
         | VNum Int
         | VBool Bool
         | Suma EAB EAB
         | Prod EAB EAB
         | Ifte EAB EAB EAB
         | Iszero EAB
         | Let String EAB EAB
         | Menor EAB EAB
         | Eq EAB EAB
         | Neg EAB
         | Asig EAB EAB 
         | Ref EAB 
         | Deref EAB 
         | L Int 
         | Seq EAB EAB 
         | While EAB EAB 
         | Void 
	 | Or EAB EAB deriving (Show,Eq)

-- Una LDir es una dirección de memoria.
-- Unicamente usaremos el caso 'L Int' del tipo EAB.
type LDir = EAB

--Usamos este alias para enfatizar que una memoria guarda valores.
type Val = EAB

--Una memoria es una lista de tuplas donde la primer entrada de cada tupla
--es una dirección de memoria y la segunda es un valor.
type Mem = [(LDir,Val)] 

{-Auxiliares semántica dinámica-}
esValor :: EAB -> Bool
esValor e = case e of
       		 VNum _ -> True
		 VBool _ -> True
		 L _ -> True
		 Void -> True
		 _ -> False
 
sust :: EAB -> String -> EAB -> EAB
sust e s r = case e of
			Var v -> if (v == s)
					then r
					else e
			VNum n -> VNum n
			VBool b -> VBool b
			Suma e1 e2 -> Suma (sust e1 s r) (sust e2 s r)
			Prod e1 e2 -> Prod (sust e1 s r) (sust e2 s r)
			Ifte e1 e2 e3 -> Ifte (sust e1 s r) (sust e2 s r) (sust e3 s r)
			Iszero e -> Iszero $ sust e s r
			l@(Let s1 e1 e2) -> if (elem s1 (fv r ++ [s]))
				then l
				else (Let s1 (sust e1 s r) (sust e2 s r))
			Menor e1 e2 -> Menor (sust e1 s r) (sust e2 s r)
			Eq e1 e2 -> Eq (sust e1 s r) (sust e2 s r)
			Neg e -> Neg (sust e s r)
			Asig e1 e2 -> Asig (sust e1 s r) (sust e2 s r)
			Ref e -> Ref (sust e s r)
			Deref e -> Deref (sust e s r)
			L n -> L n
			Seq e1 e2 -> Seq (sust e1 s r) (sust e2 s r)
			While e1 e2 -> While (sust e1 s r) (sust e2 s r)
			Void -> Void
			Or e1 e2 -> Or (sust e1 s r) (sust e2 s r)

fv :: EAB->[String]
fv e = case e of
		Var v -> [v]
		VNum n -> []
		VBool b -> []
		Suma e1 e2 -> fv e1 ++ fv e2
		Prod e1 e2 -> fv e1 ++  fv e2
		Ifte e1 e2 e3 -> fv e1 ++ fv e2 ++ fv e3
		Iszero e -> fv e
		Let x e1 e2 -> filter (/=x) (fv e1 ++ fv e2)
		Menor e1 e2 -> fv e1 ++ fv e2
		Eq e1 e2 -> fv e1 ++ fv e2
		Neg e -> fv e
		Asig e1 e2 -> fv e1 ++ fv e2
		Ref e -> fv e
		Deref e -> fv e
		L n -> []
		Seq e1 e2 -> fv e1 ++ fv e2
		While e1 e2 -> fv e1 ++ fv e2
		Void -> []
		Or e1 e2 -> fv e1 ++ fv e2

nuevaEt :: Mem -> Int
nuevaEt mem = nuevaEtAux mem 0

nuevaEtAux :: Mem -> Int -> Int
nuevaEtAux mem i = if (elem (L i) [fst x | x <- mem])
		then nuevaEtAux mem (i+1)
		else i

{-EJERCICIOS:-}
{-Semántica dinámica-}
accessMem :: LDir -> Mem->Maybe Val
accessMem  _ [] = Nothing 
accessMem (L n) ((L m,x):ms) = if (n == m)
				then Just x
				else accessMem (L n) ms

update :: LDir -> Val -> Mem -> Mem
update (L n) v mem = case mem of
			 [] -> [(L n,v)]
			 (L m,w):mem' -> if (n==m) then (L m,v):mem'  else (L m,w): (update (L n) v mem')

eval1 :: (Mem,EAB)->(Mem,EAB)
eval1 (mem,e) = case e of
		Suma (VNum n) (VNum m) -> (mem,VNum $ n+m)
		Suma v@(VNum n) e2 -> let (mem',e2') = eval1 (mem,e2) in (mem', Suma v e2')
		Suma e1 e2 -> let (mem',e1') = eval1 (mem,e1) in (mem', Suma e1' e2)
		Prod (VNum n) (VNum m) -> (mem,VNum $ n*m)
	        Prod v@(VNum n) e2 -> let (mem',e2') = eval1 (mem,e2) in (mem', Prod v e2')
	        Prod e1 e2 -> let (mem',e1') = eval1 (mem,e1) in (mem', Prod e1' e2)
		while@(While e1 e2) -> (mem,Ifte e1 (Seq e2 while) Void)
	        Ifte (VBool True) e2 e3 -> (mem,e2)
		Ifte (VBool False) e2 e3 -> (mem,e3)
		Ifte e1 e2 e3 -> let (mem',e1') = eval1 (mem,e1) in (mem', Ifte e1' e2 e3)
		Iszero (VNum n) -> (mem, VBool $ n == 0)
		Iszero e -> let (mem',e') = eval1 (mem,e) in (mem', Iszero e')
		Let s e1 e2 -> if (esValor e1) 
				then (mem, sust e2 s e1) 
				else let (mem',e1') = eval1 (mem,e1) in (mem', Let s e1' e2) 
		Menor (VNum n) (VNum m) -> (mem, VBool $ n < m) 
		Menor v@(VNum n) e2 -> let (mem',e2') = eval1 (mem,e2) in (mem', Menor v e2')
		Menor e1 e2 -> let (mem',e1') = eval1 (mem,e1) in (mem', Menor e1' e2)
		Eq (VNum n) (VNum m) -> (mem, VBool $ n == m)
		Eq v@(VNum n) e2 -> let (mem',e2') = eval1 (mem,e2) in (mem', Eq v e2')
		Eq e1 e2 -> let (mem',e1') = eval1 (mem,e1) in (mem', Eq e1' e2)
		Neg (VBool True) -> (mem, VBool False)
		Neg (VBool False) -> (mem, VBool True)
		Neg e -> let (mem',e') = eval1 (mem,e) in (mem', Neg e') 
	        Asig (L n) e2 -> if (esValor e2) 
				then (update (L n) e2 mem,Void) 
				else let (mem',e2') = eval1 (mem,e2) in (mem',Asig (L n) e2')
		Asig e1 e2 -> let (mem',e1') = eval1 (mem,e1) in (mem', Asig e1' e2)
		Ref e -> if (esValor e) 
			then ((L (nuevaEt mem),e):mem, L (nuevaEt mem))
			else let (mem',e') = eval1 (mem,e) in (mem', Ref e') 	
		Deref l@(L n) -> case accessMem l mem of
				Nothing -> error "No se encontró la dirección de memoria"
				Just v -> (mem,v)		
		Deref e -> let (mem',e') = eval1 (mem,e) in (mem', Deref e')

		Seq Void e2 -> (mem,e2)
		Seq e1 e2 -> let (mem',e1') = eval1 (mem,e1) in (mem', Seq e1' e2)
		Void -> (mem,e)
		Or (VBool False) (VBool False) -> (mem, VBool False)
		Or (VBool False) (VBool True) -> (mem, VBool True)
		Or (VBool True) e2 -> (mem, VBool True)
		Or f@(VBool False) e2 -> let (mem',e2') = eval1 (mem,e2) in (mem', Or f e2')
		Or e1 e2 -> let (mem',e1') = eval1 (mem,e1) in (mem', Or e1' e2) 		

evals :: (Mem,EAB)->(Mem,EAB)
evals (mem,e) = if (esValor e)
		then (mem,e)
		else evals(eval1(mem,e))


interp :: EAB->EAB
interp e = snd $ evals ([],e)

{-Pruebas de la semántica dinámica-}

--Debe dar true
prueba1 = interp $ let n = 1000 in Let "b" (Ref $ VBool True) $ Let "n" (Ref $ VNum n) $ Seq 
                                          (While (Neg $ Iszero (Deref $ Var "n")) $
                                          Seq (Asig (Var "b") (Neg $ Deref $ Var "b"))
                                          (Asig (Var "n") (Suma (Deref $ Var "n") (VNum $ -1))))
                                          $ Ifte (Deref $ Var "b") (VBool True) (VBool False)
                                          
--Nos dice si un n es par, en este caso n=1001
prueba2 = interp $ let n = 1001 in Let "b" (Ref $ VBool True) $ Let "n" (Ref $ VNum n) $ Seq 
                                          (While (Neg $ Iszero (Deref $ Var "n")) $
                                          Seq (Asig (Var "b") (Neg $ Deref $ Var "b"))
                                          (Asig (Var "n") (Suma (Deref $ Var "n") (VNum $ -1))))
                                          $ Ifte (Deref $ Var "b") (VBool True) (VBool False)                                          

--Calcula n!, con n=7; debe dar 5040
prueba3 = interp $ let n = 7 in Let "x" (Ref $ VNum n) $ Let "y" (Ref $ VNum 1) $ Seq (While (Menor (VNum 0) (Deref $ Var "x")) $
                                                   Seq (Asig (Var "y") (Prod (Deref $ Var "x") (Deref $ Var "y"))) 
                                                   (Asig (Var "x") (Suma (Deref $ Var "x") (VNum $ -1)))) 
                                          $ Deref $ Var "y"                                                   

--Dados n y m, calcula m/n; m=33, n=5; debe dar 6
prueba4 = interp $ let m=33 in let n=5 in Let "n" (Ref $ VNum n) $ Let "m" (Ref $ VNum m) $ Let "q" (Ref $ VNum 0) $ Let "r" (Ref $ Deref $ Var "m") $ 
                                                   Seq (While (Or (Eq (Deref $ Var "r") (Deref $ Var "n")) (Menor (Deref $ Var "n") (Deref $ Var "r"))) $
                                                   Seq (Asig (Var "q") (Suma (Deref $ Var "q") (VNum 1)))
                                                   (Asig (Var "r") (Suma (Deref $ Var "r") (Prod (Deref $ Var "n") (VNum $ -1)))))
                                                   $ Deref $ Var "q"
                                                   
--Dados n y m, calcula el residuo de m/n; m=40, n=6; debe dar 4
prueba5 = interp $ let m=40 in let n=6 in Let "n" (Ref $ VNum n) $ Let "m" (Ref $ VNum m) $ Let "q" (Ref $ VNum 0) $ Let "r" (Ref $ Deref $ Var "m") $ 
                                                   Seq (While (Or (Eq (Deref $ Var "r") (Deref $ Var "n")) (Menor (Deref $ Var "n") (Deref $ Var "r"))) $
                                                   Seq (Asig (Var "q") (Suma (Deref $ Var "q") (VNum 1)))
                                                   (Asig (Var "r") (Suma (Deref $ Var "r") (Prod (Deref $ Var "n") (VNum $ -1)))))
                                                   $ Deref $ Var "r"
                                                   
--Calcula el máximo común divisor de m y n; m=2366, n=273; debe dar 91
prueba6 = interp $ let m=2366 in let n=273 in Let "n" (Ref $ VNum n) $ Let "m" (Ref $ VNum m) $ Let "r" (Ref $ Deref $ Var "m") $Seq (While (Neg $ Iszero $ Deref $ Var "n") $
                                                   Seq (Asig (Var "r") (Deref $ Var "m")) $
                                                   Seq (While (Or (Eq (Deref $ Var "r") (Deref $ Var "n")) (Menor (Deref $ Var "n") (Deref $ Var "r"))) $
                                                   Asig (Var "r") (Suma (Deref $ Var "r") (Prod (Deref $ Var "n") (VNum $ -1)))) $
                                                   Seq (Asig (Var "m") (Deref $ Var "n")) $
                                                   Asig (Var "n") (Deref $ Var "r"))
                                                   $ Deref $ Var "m"



{-Semántica estática-}

data Tipo = TInt | TBool | TVoid | TRef Tipo deriving (Show,Eq)



--Los contextos ahora incluyen un conjunto exclusivo para direcciones de memoria.
type Ctx = ([(String,Tipo)],[(LDir,Tipo)])


vt :: Ctx->EAB->Tipo
vt ctx e = case e of
		 Var x -> case ctx of
		  	([],ctxL) -> error $ "La variable "++show x++" no está en el contexto"
		  	((y,t):ctxV,ctxL) -> if x==y 
						then t 
						else vt (ctxV,ctxL) e
		 VNum _ -> TInt
		 VBool _ -> TBool
		 Suma e1 e2 -> let 
				t1 = vt ctx e1 
				t2 = vt ctx e2 in
					if (t1==TInt && t2==TInt) 
					then TInt 
					else (if (t1/=TInt) 
						then error $ "La expresión "++ show e1 ++" no es de tipo TInt" 
						else  error $ "La expresión "++ show e2 ++" no es de tipo TInt")
		 Prod e1 e2 -> let 
				t1 = vt ctx e1 
				t2 = vt ctx e2 in
					if (t1==TInt && t2==TInt) 
					then TInt 
					else (if (t1/=TInt) 
						then error $ "La expresión "++ show e1 ++" no es de tipo TInt" 
						else  error $ "La expresión "++ show e2 ++" no es de tipo TInt")
		 Ifte e1 e2 e3 -> let
				   t1 = vt ctx e1
				   t2 = vt ctx e2
				   t3 = vt ctx e3 in
					if (t1 == TBool && t2 == t3) 
					then t2 
					else (if (t1/=TBool)
						then error $ "La expresión "++ show e1 ++" no es de tipo TBool"
						else error $ "Las expresiones "++ show e2 ++ " y "++ show e3 ++" no son del mismo tipo")
		 Iszero e -> let
				t = vt ctx e in
					if (t == TInt)
					then TBool
					else error $ "La expresión "++ show e ++" no es de tipo TInt"
		 Let s e1 e2 -> let 
				t1 = vt ctx e1 in 
						vt ((s,t1):(fst ctx),snd ctx) e2
		 Menor e1 e2 -> let
				t1 = vt ctx e1
				t2 = vt ctx e2 in
					if (t1 == TInt && t2 == TInt)
					then TBool
					else (if (t1 /= TInt)
						then error $ "La expresión "++ show e1 ++" no es de tipo TInt"
						else error $ "La expresión "++ show e2 ++" no es de tipo TInt")
		 Eq e1 e2 -> let
				t1 = vt ctx e1
				t2 = vt ctx e2 in
					if (t1 == TInt && t2 == TInt)
					then TBool
					else (if (t1 /= TInt)
						then error $ "La expresión "++ show e1 ++" no es de tipo TInt"
						else error $ "La expresión "++ show e2 ++" no es de tipo TInt")
		 Neg e -> let
			    t = vt ctx e in
				if (t == TBool)
				then TBool
				else error $ "La expresión "++ show e ++" no es de tipo TBool"
		 Asig e1 e2 -> let
				TRef t1 = vt ctx e1 
				t2 = vt ctx e2 in 
					if (t2 == t1)
					then TVoid
					else error $ "La expresion  "++ show e1 ++" no es de tipo TRef" 
		 Ref e -> let t = vt ctx e in TRef t
		 Deref e -> let
				TRef t = vt ctx e in t	
						
		 L n -> case ctx of
			(ctxV,[]) -> error $ "La dirección de memoria " ++ show n ++ " es inválida"
		        (ctxV,(L m,l):ctxL) -> if  (n == m) 
						then TRef l 
						else vt (ctxV,ctxL) e 
		 Seq e1 e2 -> let
				t1 = vt ctx e1 
				t2 = vt ctx e2 in
					if (t1 == TVoid)
					then t2
					else error $ "La expresión "++ show e1 ++" no es de tipo TVoid"
		 While e1 e2 -> let
				t1 = vt ctx e1 
				t2 = vt ctx e2 in
					if(t1 == TBool && t2 == TVoid)
					then TVoid
					else (if (t1 /= TBool)
							then error $ "La expresión "++ show e1 ++" no es de tipo TBool"
							else error $ "La expresión "++ show e2 ++" no es de tipo TVoid")
		 Or e1 e2 -> let
				t1 = vt ctx e1
				t2 = vt ctx e2 in
					if(t1 == TBool && t2 == TBool)
					then TBool
					else (if (t1 /= TBool)
							then error $ "La expresión "++ show e1 ++" no es de tipo TBool"
							else error $ "La expresión "++ show e2 ++" no es de tipo TBool")
		 Void -> TVoid

{-Pruebas de la semántica estática-}
--Debe dar TVoid
prueba7 = vt ([("y",TRef TInt)],[(L 0,TInt)]) $ Let "x" (Ref (VNum 3)) (Seq (Asig (Var "x") (Suma (Deref(L 0)) (Deref (Var "y")))) (Asig (Var "y") (Prod (VNum 3) (Deref(Var "x")))))
--Debe dar TBool
prueba8 = vt ([("x",TRef TInt),("y",TRef TBool)],[]) $ Ifte (Menor (Deref (Var "x")) (Prod (Suma (VNum 3) (Deref (Var "x"))) (VNum 4))) (Or (Iszero (Deref(Var "x"))) (Deref (Var "y"))) (Eq (Suma (Prod (Deref(Var "x")) (VNum 5)) (Deref(Var "x"))) (Deref(Var"x")))
--Debe dar TVoid
prueba9 = vt ([("x",TRef TInt),("y",TRef TInt),("z",TRef TBool)],[]) $ While (Iszero (Ifte (Deref(Var "z")) (Suma (Deref(Var "x")) (VNum 9)) (Prod (Deref(Var "y")) (VNum 9)))) (Let "z" (Ref (Deref (Var "x"))) (Seq (Asig (Var "x") (Deref (Var "y"))) (Asig (Var "y") (Deref (Var "z")))))
--Debe dar "La dirección de memoria 0 es inválida"
prueba10 = vt ([("x",TRef TInt)],[(L 1,TBool),(L 2, TInt)]) $ While (Neg (Menor (Deref(Var "x")) (VNum 5))) (Seq (Suma (L 0) (Deref(Var "x"))) (Or (Neg (L 1)) (Menor (L 2) (Prod (Deref(Var "x")) (VNum 2)))))
--Debe dar "Las expresiones L 1 y L 0 no son del mismo tipo"
prueba11 = vt ([("x",TRef TInt)],[(L 0,TInt),(L 1,TBool)]) $ Let "y" (Neg (Neg (Or (Menor (Deref (Var "x")) (Deref(L 0))) (Deref(L 1))))) (Ifte (Neg (Var "y")) (L 1) (L 0))


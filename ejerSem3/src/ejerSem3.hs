{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-1 
              Profesor: Dr. Favio Ezequiel Miranda 
              Ayudante: Susana Hahn Martín Lunas
              Laboratorio: C. Moisés Vázquez Reyes-}



infixr :-> {- Así, el poderador ':->' asocia a la derecha. -}
type Nombre = Int 


-- Categoría de tipos.
data Tipo = TNat | TBool | X Nombre | Tipo :-> Tipo deriving Eq


instance Show Tipo where
     show t = case t of
            TNat -> "Nat"
            TBool -> "Bool"
            X i -> "X"++show i 
            TNat:->TNat -> "Nat" ++"->"++"Nat"
            TNat:->TBool -> "Nat" ++"->"++"Bool"
            TNat:->(X i) -> "Nat"++"->"++"X"++show i
            TNat:->(t1:->t2) -> "Nat"++"->("++show t1++"->"++show t2++")"
            TBool:->TBool -> "Bool" ++"->"++"Bool"
            TBool:->TNat -> "Bool" ++"->"++"Nat"
            TBool:->(X i) -> "Bool"++"->"++"X"++show i
            TBool:->(t1:->t2) -> "Bool"++"->("++show t1++"->"++show t2++")"
            (X i):->TNat -> "X"++show i++"->"++"Nat"
            (X i):->TBool -> "X"++show i ++"->"++"Bool"
            (X i):->(X j) -> "X"++show i++"->"++"X"++show j
            (X i):->(t1:->t2) -> "X"++show i++"->("++show t1++"->"++show t2++")"
            (t1:->t2):->TNat -> "("++show t1++"->"++show t2++")"++"->"++"Nat"
            (t1:->t2):->TBool -> "("++show t1++"->"++show t2++")"++"->"++"Bool"
            (t1:->t2):->(X i) -> "("++show t1++"->"++show t2++")"++"->"++"X"++show i
            (t1:->t2):->(t3:->t4) -> "("++show t1++"->"++show t2++")"++"->("++show t3++"->"++show t4++")"



--Una sustitución es un conjunto de la forma [(xi, Ti)]
type Sust = [(Nombre, Tipo)]

--Para pintar una sustitución bonito.
pintaSust :: Sust->String
pintaSust s = show $ map (\(x,y) -> (X x, y)) s 



{-EJERCICIOS:-}

--Elimina sustituciones de la forma ("n", X "n") en una sustitución.
simpSust :: Sust->Sust
simpSust [] = []
simpSust ((x,t):xs) = case t of
			X y -> if x == y
				  then simpSust xs
				  else (x,t):(simpSust xs) 
                        t -> (x,t):(simpSust xs)

--Realiza la composición de dos sustituciones.
compSust :: Sust->Sust->Sust
compSust [] s = s
compSust ((s,t):st) s1 = simpSust $ [(s,(apSustT t s1))] ++ (compSust st s1)
 

--Aplica una sustitución a un tipo.
apSustT :: Tipo->Sust->Tipo 
apSustT t s = let s' = simpSust s in case t of
				TNat -> TNat
				TBool -> TBool
				X x -> case s' of
			  		  [] -> X x
			  		  ((y,r):ys) -> if x == y
					 	        then r 
					      	 	else apSustT t ys
				t1 :-> t2 -> (apSustT t1 s') :-> (apSustT t2 s')

--Unifica dos tipos.
unifica :: Tipo->Tipo->[Sust]
unifica (X x) (X y) = if x == y
			then [[]]
			else [[(x,X y)]]
unifica t (X x) = unifica (X x) t
unifica (X x) t = if (elem x (varT t)) 
		  then error "]"
 		  else [[(x,t)]] where 
			varT t = elimRep $ varT_ t where 
			varT_ t = case t of
				X x -> [x]
			        t1 :-> t2 -> varT_ t1 ++ varT_ t2
				_ -> []      
unifica (t1:->t2) (s1:->s2) = let sust1 = head $ unifica t1 s1
				  sust2 = head $ unifica (apSustT t2 sust1) (apSustT s2 sust1)
			      in
				  [compSust sust1 sust2] 

elimRep [] = []
elimRep (t:ts) = t:(filter (/=t) $ elimRep ts)

--Unifica una lista de tipos.
unificaConj :: [Tipo]->[Sust]
unificaConj [] = []
unificaConj (t:[]) = []
unificaConj (t:ts) = if (t == head ts)
			then [[]] ++ unificaConj ts
			else let s = (unifica t (head ts)) 
				in [compSustLista (s ++ (unificaConj (apSustTipo (head s) (tail ts))))]

apSustTipo :: Sust->[Tipo]->[Tipo]
apSustTipo s t = case t of
		[] -> []
		(t:ts) -> [apSustT t s] ++ apSustTipo s ts

compSustLista :: [Sust]->Sust
compSustLista t = case t of
		[] -> []
		(t:ts) -> compSust t (compSustLista ts)

{-PRUEBAS:-}
--[[]]
prueba1 = map pintaSust $ unificaConj [((X 0):->((X 1):->(X 1)):->(X 0))]

--[[(X2,X4->X7),(X3,X4->X7)]]
prueba2 = map pintaSust $ unificaConj [X 2,X 3,X 2,(X 4):->(X 7),X 4]

--[[(X0,Nat->(X1->X1)),(X5,X1->X1),(X6,Nat->(X1->X1))]]
prueba3 = map pintaSust $ unificaConj [((TNat:->(X 5)):->((X 5):->(X 6))) ,((X 0):->((X 1):->(X 1)):->(X 0))]

--[[("X3,X0->X1),(X2,Nat),(X6,Bool),(X8,Nat->X4)]]
prueba4 = map pintaSust $ unificaConj [((X 0):->(X 1)):->TNat,(X 3):->(X 2),(TBool):->(X 8),(X 6):->((TNat):->(X 4))]

--[]
prueba5 = map pintaSust $ unificaConj [(X 5):->(X 1):->(X 5),(X 1):->((X 5):->TNat):->(X 5)]

prueba8 = map pintaSust $ unifica (((TNat):->(X 1)):->((X 1):->(X 2)))((X 3):->((X 4):->(X 4)):->(X 3)) 




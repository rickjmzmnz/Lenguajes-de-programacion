{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-1 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: Susana Hahn Martín Lunas
		  Laboratorio: C. Moisés Vázquez Reyes-}

   {-ÍNDICES DE DE BRUJIN-}
   
--Expresiones del cálculo lambda
data E = Var String | App E E | Lambda String E deriving Show

--Términos anónimos
data A = VarA Int | AppA A A | LambdaA A deriving Show 

--Contexto canónico
type CtxCan = [(String,Int)]

--Contexto de nombres
type CtxNom = [String]

ind :: String -> CtxNom -> Int -> Int
ind _ [] n = error ""
ind x xs n  = if (x == (last xs))
		 then n
		 else ind x (init xs) (n+1)

indCan :: Int -> CtxCan -> String
indCan n [] = [x | x <- take n ['a'..'z']]
indCan x ((s,i):l) = if (x == i)
			then s
			else indCan x l

--Ejercicios:
--Función que transforma una expresión a un término anónimo.
qn::CtxNom->E->A 
qn ctx (Var x) = VarA (ind x ctx 0) 
qn ctx (Lambda s e) = LambdaA (qn (ctx ++ [s]) e)
qn ctx (App e1 e2) = AppA (qn ctx e1) (qn ctx e2)

--Función que transforma un término anónimo en una expresión lambda.
pn::CtxCan->A->E
pn ctx (VarA a) = Var (indCan a ctx)
pn ctx (AppA a1 a2) = App (pn ctx a1) (pn ctx a2)
--pn ctx (LambdaA e) = Lambda (indCan (snd ctx) ctx) (pn (ctx:[(indCan (snd ctx) ctx),(snd ctx)]) e)

--Función que desplaza índices.
shift::Int->Int->A->A  
shift n c (VarA m) = if (m < c)
			   then VarA m 
			 else VarA (n+m)
shift n c (AppA a b) = AppA (shift n c a) (shift n c b)
shift n c (LambdaA a) = LambdaA (shift n (c+1) a)


--Aplica una sustitución a un término anónimo.
sust::A->Int->A->A
sust (VarA a) n  s = if (a == n)
		     then s	
		     else (VarA a)
sust (AppA a1 a2) n s = AppA (sust a1 n s) (sust a2 n s)
sust (LambdaA a) n s = LambdaA (sust a (n+1) (shift 1 0 s))	

--Realiza la beta reducción.
br::A->A->A
br (LambdaA a) s = shift (-1) 0 (sust a 0 (shift 1 0 s)) 

--Debe dar AppA (LambdaA (LambdaA (AppA (AppA (VarA 3) (VarA 2)) (VarA 0)))) (LambdaA (AppA (AppA (VarA 2) (VarA 1)) (VarA 0)))
prueba1 = br (LambdaA (AppA (LambdaA (VarA 1)) (VarA 0))) (LambdaA (AppA (AppA (VarA 2) (VarA 1)) (VarA 0)))













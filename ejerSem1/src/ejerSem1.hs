{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-1 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: Susana Hahn Martín Lunas
		  Laboratorio: C. Moisés Vázquez Reyes-}

--Paréntesis balanceados.
data M = Em | Par M | ConctM M M 
data L = El | ConctP L L 

--Hacemos que los tipos M y L formen parte de la clase Show.
instance Show M where
	show m = case m	of	
		Em -> ""
		Par m -> "("++show m++")"
		ConctM m1 m2 -> show m1 ++ show m2

instance Show L where
	show l = case l of
		El -> ""
		ConctP l1 l2 -> "("++show l1++")" ++ show l2

--Concatena dos cadenas de L.
conctL :: L->L->L
conctL El m = m
conctL (ConctP n1 n2) m = ConctP n1 (conctL n2 m)

--Convierte cadenas de M en L.
mToL :: M->L
mToL Em = El
mToL (Par m) = ConctP (mToL m) El
mToL (ConctM n m) = conctL (mToL n) (mToL m)

--Convierte cadenas de L en M.
lToM :: L->M
lToM El = Em
lToM (ConctP l El) = Par (lToM l)
lToM (ConctP l1 l2) = ConctM (Par (lToM l1)) (lToM l2)

--Tipo de dato para implementar el lexer.
data Tokens = ParA | ParC | Desc deriving (Show, Eq)


--Lexer que recibe una cadena de texto y lo convierte en tokens.
lexer :: String->[Tokens]
lexer "" = []
lexer ('(':ls) = ParA:(lexer ls)
lexer (')':ls) = ParC:(lexer ls)
lexer (_:ls) = lexer ls

--Tipo de dato para representar los juicios de análisis sintáctico.
data Pila = Pila Int [Tokens] 

--Función que hace un análisis sintáctico para determinar si una cadena está balanceada o no.
analiSintc :: Pila->Bool
analiSintc (Pila 0 []) = True
analiSintc (Pila k (ParA:ts)) = analiSintc (Pila (k+1) ts)
analiSintc (Pila k (ParC:ts)) = analiSintc (Pila (k-1) ts)
analiSintc (Pila k (_:ts)) = analiSintc (Pila k ts)
analiSintc (Pila k []) = False

--Función que determina si una cadena está formada por paréntesis y está balanceada.
esBalanceada :: String->Bool
esBalanceada s = analiSintc (Pila 0 (lexer s)) 

--Función que convierte una cadena de texto balanceada en un objeto de tipo M. 
parserM :: String->M
parserM "" = Em
parserM ms = if esBalanceada ms 
		then if esBalanceada (take 2 ms)
			then ConctM (Par Em) (parserM (drop 2 ms))
			else (sumaM 4 ms)
		else error "cadena desbalanceada"

--Función auxiliar para parserM.
--Cuando tenemos que verificar los siguientes dos caracteres de la cadena
--Y ver si la cadena esta balanceada. 
sumaM :: Int->String->M
sumaM n ms = if esBalanceada (take n ms)
		then ConctM (Par (parserM ((tail.init) (take n ms)))) (parserM (drop n ms))
		else sumaM (n+2) ms

  
--Función que convierte una cadena de texto balanceada en un objeto de tipo L.
parserL :: String->L
parserL "" = El
parserL ls = if esBalanceada ls 
		then if esBalanceada (take 2 ls)
			then ConctP El (parserL (drop 2 ls))
			else (sumaL 4 ls)
		else error "cadena desbalanceada"

--Función auxiliar para parserL.
--Cuando tenemos que verificar los siguientes dos caracteres de la cadena
--Y ver si la cadena esta balanceada. 
sumaL :: Int->String->L
sumaL n ls = if esBalanceada (take n ls)
		then ConctP (parserL ((tail.init) (take n ls))) (parserL (drop n ls))
		else sumaL (n+2) ls
			
--PRUEBAS:
prueba1 = show (Par $ ConctM (Par $ Par Em) (Par Em)) == "((())())"
prueba2 = show (ConctP (ConctP (ConctP El El) (ConctP El El)) El)  == "((())())" 
prueba3 = show (conctL (parserL "()()()") (parserL "(())")) == "()()()(())"  
prueba4 = show (mToL $ parserM "(())()(())") == "(())()(())" 
prueba5 = show (lToM $ parserL "(())()(())()") == "(())()(())()"
prueba6 = lexer (show $ parserM "(()((())(())))") == 
                              [ParA,ParA,ParC,ParA,ParA,ParA,ParC,ParC,ParA,ParA,ParC,ParC,ParC,ParC]
prueba7 = lexer (show $ parserL "(()((())(())))") == 
                              [ParA,ParA,ParC,ParA,ParA,ParA,ParC,ParC,ParA,ParA,ParC,ParC,ParC,ParC]
prueba8 = analiSintc $ Pila 0 $ lexer "(()((())(())))"
--Sólo estas pruebas deben devolver False.
prueba9 = analiSintc $ Pila 0 $ lexer "(()((())(()))"
prueba10 = analiSintc $ Pila 0 $ lexer "(()((())())))"





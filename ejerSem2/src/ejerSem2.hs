{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-1 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: Susana Hahn Martín Lunas
		  Laboratorio: C. Moisés Vázquez Reyes-}

--Sintaxis concreta de EA (Primer propuesta)
data Exp = Num N | Suma Exp Exp | Prod Exp Exp 
data N = Dig D | ConsN N D 
data D = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 


--Función de evaluación de expresiones EA
evalEA1 :: Exp->Int
evalEA1 (Num n) = num n
evalEA1 (Suma n m) = (evalEA1 n) + (evalEA1 m)
evalEA1 (Prod n m) = (evalEA1 n) * (evalEA1 m)

num :: N->Int
num (Dig d) = dig d
num (ConsN n d) = (num n) * 10 + (dig d) 

dig :: D->Int
dig D0 = 0
dig D1 = 1
dig D2 = 2
dig D3 = 3
dig D4 = 4
dig D5 = 5
dig D6 = 6
dig D7 = 7
dig D8 = 8
dig D9 = 9

--Sintaxis concreta de EA (Segunda propuesta)
data E = Term T | SumE E T           --Una expresión es un término o la suma de una expresión y un término.
data T = Fact F | ProdT T F          --Un término es un factor o el producto de un término y un factor.
data F = Numero NumF | P E           --Un factor es un número o una expresión parentisada.
data NumF = DigN D_ | ConsF NumF D_  --Un número es un dígito o un número seguido de un dígito.
data D_ = D_0 | D_1 | D_2 | D_3 | D_4 | D_5 | D_6 | D_7 | D_8 | D_9 


--Función de evaluación de expresiones EA
evalEA2 :: E->Int
evalEA2 (Term t) = term t
evalEA2 (SumE e t) = (evalEA2 e) + (term t) 

term :: T->Int
term (Fact f) = fact f
term (ProdT t f) = (term t) * (fact f)

fact :: F->Int
fact (Numero n) = numF n
fact (P e) = evalEA2 e

numF :: NumF->Int
numF (DigN d) = dig_ d
numF (ConsF n d) = (numF n) * 10 + (dig_ d) 

dig_ :: D_->Int
dig_ D_0 = 0
dig_ D_1 = 1
dig_ D_2 = 2
dig_ D_3 = 3
dig_ D_4 = 4
dig_ D_5 = 5
dig_ D_6 = 6
dig_ D_7 = 7
dig_ D_8 = 8
dig_ D_9 = 9
 
{-Un estado es una lista de tuplas donde se indica el valor de cada variable.-}
type Est = [(String, Int)]
type V = String

{-Aquí va tu modificación de la segunda gramática de tal manera que acepte variables.-} 
data E_ = Term_ T_ | SumE_ E_ T_           --Una expresión es un término o la suma de una expresión y un término.
data T_ = Fact_ F_ | ProdT_ T_ F_          --Un término es un factor o el producto de un término y un factor.
data F_ = Numero_ NumF_ | P_ E_ | Var V          --Un factor es un número o una expresión parentisada.
data NumF_ = DigN_ D__ | ConsF_ NumF_ D__  --Un número es un dígito o un número seguido de un dígito.
data D__ = D__0 | D__1 | D__2 | D__3 | D__4 | D__5 | D__6 | D__7 | D__8 | D__9

{-Esta es la función que evalúa expresiones EA con variables. Debes descomentar
la función y el nombre del primer tipo debe coincidir con tu gramática.-}
evalEA3 :: E_->Est->Int
evalEA3 (Term_ t) est = term_ t est
evalEA3 (SumE_ e t) est = (evalEA3 e est) + (term_ t est) 

term_ :: T_->Est->Int
term_ (Fact_ f) est = fact_ f est
term_ (ProdT_ t f) est = (term_ t est) * (fact_ f est)

fact_ :: F_->Est->Int
fact_ (Numero_ n) _ = numF_ n
fact_ (P_ e) est = evalEA3 e est
fact_ (Var v) ((s,n):ns) = if v == s 
				then n 
o				else (fact_ (Var v) ns)
fact_ (Var v) [] = error "estado vacio"     

numF_ :: NumF_->Int
numF_ (DigN_ d) = dig__ d
numF_ (ConsF_ n d) = (numF_ n) * 10 + (dig__ d)

dig__ :: D__->Int
dig__ D__0 = 0
dig__ D__1 = 1
dig__ D__2 = 2
dig__ D__3 = 3
dig__ D__4 = 4
dig__ D__5 = 5
dig__ D__6 = 6
dig__ D__7 = 7
dig__ D__8 = 8
dig__ D__9 = 9

{-PRUEBAS-}
--10*(258+47+369) = 6740
prueba1 = evalEA1 $ Prod (Num $ ConsN (Dig D1) D0) (Suma (Suma (Num $ ConsN (ConsN (Dig D2) D5) D8) (Num $ ConsN (Dig D4) D7)) (Num $ ConsN (ConsN (Dig D3) D6) D9))
--1000 + ( 300*2 + 10) = 1610
prueba2 = evalEA1 $ Suma (Num $ ConsN (ConsN (ConsN (Dig D1) D0) D0) D0) (Suma (Prod (Num $ ConsN (ConsN (Dig D3) D0) D0) (Num $ Dig D2)) (Num $ ConsN (Dig D1) D0))
--3*699 + 2 = 2099
prueba3 = evalEA2 $ SumE (Term $ ProdT (Fact $ Numero $ DigN D_3) (Numero $ ConsF (ConsF (DigN D_6) D_9) D_9)) (Fact $ Numero $ DigN D_2)
--(154+8016)*(299+734) = 8439610
prueba4 = evalEA2 $ Term $ ProdT (Fact $ P $ SumE (Term $ Fact $ Numero $ ConsF (ConsF (DigN D_1) D_5) D_4) (Fact $ Numero $ ConsF (ConsF (ConsF (DigN D_8) D_0) D_1) D_6)) (P $ SumE (Term $ Fact $ Numero $ ConsF (ConsF (DigN D_2) D_9) D_9) (Fact $ Numero $ ConsF (ConsF (DigN D_7) D_3) D_4))
{-Estas pruebas debes implementarlas usando tu gramática modificada.-}
--64+(345*x)+y=33530 con el estado [y=1, x=97]
prueba5 = evalEA3 (SumE_ (SumE_ (Term_ $ Fact_ $ Numero_ $ ConsF_ (DigN_ D__6) D__4 ) (Fact_ $ P_ (Term_ (ProdT_ (Fact_ $ Numero_ $ ConsF_ (ConsF_ (DigN_ D__3) D__4) D__5) (Var "x"))))) (Fact_ $ Var "y")) [("y",1),("x",97)]

--x*88*(7+z+y) = 143616 con el estado [x=102, z=4, y=5]
prueba6 = evalEA3 (Term_ (ProdT_ ((ProdT_ (Fact_ $ Var "x") (Numero_ $ ConsF_ (DigN_ D__8) D__8))) (P_ (SumE_ (SumE_ (Term_ $ Fact_ $ Numero_ $ DigN_ D__7) (Fact_ $ Var "z")) (Fact_ $ Var "y"))))) [("x",102),("z",4),("y",5)]


--Definición de ParNat
data DNat = Cero | D DNat | U DNat deriving Show


--Función que simplifica la representación de un DNat.
simplDN :: DNat -> DNat
simplDN Cero = Cero
simplDN (U n) = U $ simplDN n
simplDN (D Cero) = Cero
simplDN (D n) = aux $ D $ simplDN n where
		aux (D Cero) = Cero
		aux (D n) = D $ simplDN n
 

--Función sucesor en DNat.
sucDN :: DNat->DNat
sucDN n = simplDN $ aux_suc n where
		    aux_suc Cero = (U Cero)
		    aux_suc (D n) = (U n)
		    aux_suc (U n) = D $ sucDN n
          

--Función que calcula el predecesor de un número DNat.
predDN :: DNat->DNat
predDN n = simplDN $ aux_pred n where
		     aux_pred Cero = Cero
		     aux_pred (U n) = (D n)
--                   Forma distinta de implementar ese caso   
--		     aux_pred (D n) = sucDN $ D $ predDN n
		     aux_pred (D n) = U $ predDN n


--Función que nos da la representación de un número DNat en los números enteros.
dNToZ :: DNat->Int
dNToZ Cero = 0
dNToZ (U Cero) = 1
dNToZ (D Cero) = 0
dNToZ (D n) = 2 * dNToZ n
dNToZ (U n) = ((2 * dNToZ n)+1)

--Función que suma dos números DNat.
sumaDN :: DNat->DNat->DNat
sumaDN n m = simplDN $ aux_suma n m where
		       aux_suma n Cero = n
		       aux_suma Cero n = n
		       aux_suma (D n) (D m) = D $ sumaDN n m
		       aux_suma (U n) (D m) = U $ sumaDN n m
		       aux_suma (D n) (U m) = U $ sumaDN n m
		       aux_suma (U n) (U m) = D $ sucDN (sumaDN n m)
--                     Función alterna para la suma   
--		       aux_suma n Cero = n
--	               aux_suma n m = sumaDN (sucDN n) (predDN m)
		       
--Función que multiplica dos números DNat.
prodDN :: DNat->DNat->DNat
prodDN n m = simplDN $ aux_prod n m where
		       aux_prod _ Cero = Cero
		       aux_prod Cero _ = Cero
		       aux_prod n (U Cero) = n
		       aux_prod (U Cero) n = n
		       aux_prod (D n) (D m) = D $ D $ prodDN n m
		       aux_prod (D n) (U m) = D $ sumaDN (D $ prodDN n m) n
		       aux_prod (U n) (D m) = D $ sumaDN (D $ prodDN n m) m
		       aux_prod (U n) (U m) = U $ sumaDN (sumaDN (D $ prodDN n m) n) m
--		       Función alterna para el producto
--		       aux_prod n Cero = Cero
--		       aux_prod n m = sumaDN n (prodDN n (predDN m))

--Función que transforma un entero positivo a su representación en DNat.
zToDNat :: Int->DNat
zToDNat 0 = Cero
zToDNat n = sucDN (zToDNat (n-1)) 
	
--PRUEBAS:

--Debe dar 31
prueba1 = dNToZ $ sucDN $ sumaDN (D $ D $ U $ U $ D $ D Cero) (predDN $ zToDNat 19)
--Debe dar 5844
prueba2 = dNToZ $ sucDN $ sucDN $ prodDN (U $ U $ U $ D $ U $ D Cero) (sumaDN (U $ D $ U Cero) (zToDNat 249))
--Debe dar 21
prueba3 = (dNToZ $ sumaDN (U $ U $ D $ D $ D $ Cero) (U $ U $ D Cero)) + (dNToZ $ sucDN $ D $ U $ U $ U $ D Cero)
--Debe dar 38
prueba4 = dNToZ $ zToDNat $ dNToZ $ sumaDN (U $ U $ D $ D $ D $ U Cero) (U $ U $ D $ D $ D Cero)

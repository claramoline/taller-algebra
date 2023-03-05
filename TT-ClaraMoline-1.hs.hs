
--Ejercicio 1

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n |  n > 2 && esPar n && esSumaPrimos n = True
                    | otherwise = False

--En una sola línea

satisfaceGoldbach1 :: Integer -> Bool 
satisfaceGoldbach1 n = n>2 && esPar n && esSumaPrimos n                   


--Auxiliares:                    

esPar :: Integer -> Bool
esPar n | n `mod` 2 == 0 = True
        | otherwise = False


esPrimoRec :: Integer -> Integer -> Bool
esPrimoRec n k |  k == n = True
               |  n `mod` k == 0 = False
               | otherwise = esPrimoRec n (k+1)


esPrimo :: Integer -> Bool
esPrimo n | esPrimoRec n 2 = True
          |otherwise = False


esSumaPrimosRec :: Integer -> Integer -> Bool
esSumaPrimosRec n k | k == n = False 
                    | esPrimo k && esPrimo (n-k) = True
                    | otherwise = esSumaPrimosRec n (k+1)


esSumaPrimos :: Integer -> Bool
esSumaPrimos n | esSumaPrimosRec n 2 = True
               | otherwise = False


--Ejercicio 2 

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta 4 = True 
verificarConjeturaHasta n = satisfaceGoldbach n && verificarConjeturaHasta (n-2) 



--Ejercicio 3

descomposicionEnPrimos :: Integer -> (Integer, Integer)  
descomposicionEnPrimos n | satisfaceGoldbach n == False = undefined
                         | otherwise = descomposicionEnPrimosRec n 2 


--Auxiliares:

descomposicionEnPrimosRec :: Integer -> Integer -> (Integer, Integer)
descomposicionEnPrimosRec n k | esPrimo k && esPrimo (n-k) = (k, n-k)
                              | otherwise = descomposicionEnPrimosRec n (k+1)


--Ejercicio 4: --eliminé una función auxiliar para hacerlo más prolijo

numeroDeDescomposiciones :: Integer -> Integer 
numeroDeDescomposiciones n = buscarPares n 2 
                           


buscarPares :: Integer -> Integer -> Integer
buscarPares n k | k == n = 0
                | (esPrimo k) && (esPrimo (n-k)) = buscarPares n (k+1) + 1
                | otherwise = buscarPares n (k+1)                







                        
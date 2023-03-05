type Posicion = [Int]
type Jugada = (Int,Int)
type Set a = [a]

--Ejercicio 1:

jugar :: Posicion -> Jugada -> Posicion --A partir de una posición y una jugada válida devuelve una posición. 
jugar [] (_,_) = []
jugar p j = sacarDePila p j

sacarDePila :: Posicion -> Jugada -> Posicion                 
sacarDePila (x:xs) (n,m) | n == 1 && (x-m) == 0  = xs 
                         | n == 1 && (x-m) /= 0  = (x-m):xs 
                         | otherwise = x:(sacarDePila xs (n-1,m)) 

--Ejercicio 2: 

posiblesJugadas :: Posicion -> [Jugada] --Recibe una posición y devuelve la lista de jugadas válidas de esa posición.
posiblesJugadas [] = []
posiblesJugadas p = jugadasValidas p 1 1 

jugadasValidas :: Posicion -> Int -> Int -> [Jugada]
jugadasValidas p m n | n > cantPilas p = []
                     | m > piedrasEnPila n p = jugadasValidas p 1 (n+1)
                     | otherwise = (n,m) : jugadasValidas p (m+1) n

cantPilas :: Posicion -> Int 
cantPilas [] = 0
cantPilas (x:xs) = 1 + cantPilas xs                   

piedrasEnPila :: Int -> Posicion -> Int 
piedrasEnPila n (x:xs) | n == 1 = x
                       | otherwise = piedrasEnPila (n-1) xs

--Ejercicio 3: 

esPosicionGanadora :: Posicion -> Bool --Evalúa si la posición es ganadora
esPosicionGanadora [] = False
esPosicionGanadora p = hallaPerdedoraEn p (posiblesJugadas p)

hallaPerdedoraEn :: Posicion -> [Jugada] -> Bool
hallaPerdedoraEn p [] = False
hallaPerdedoraEn p (x:xs) | not (esPosicionGanadora (jugar p x)) = True
                          | otherwise = hallaPerdedoraEn p xs

--Ejercicio 4:

jugadaGanadora :: Posicion -> Jugada --Recibe una posicíón ganadora y devuelve una jugada que haría que el rival pierda.
jugadaGanadora p = devuelvePosPerdedora p (posiblesJugadas p) 

devuelvePosPerdedora :: Posicion -> [Jugada] -> Jugada
devuelvePosPerdedora p (x:xs) | not (esPosicionGanadora (jugar p x)) = x
                              | otherwise = devuelvePosPerdedora p xs

--Ejercicio 5: 

numeroDeJugadasGanadoras :: Posicion -> Int --Recibe una posición y devuelve la cantidad de jugadas ganadoras partiendo de esa posición.
numeroDeJugadasGanadoras p = todasLasGanadoras p (posiblesJugadas p)

todasLasGanadoras :: Posicion -> [Jugada] -> Int
todasLasGanadoras _ [] = 0
todasLasGanadoras p (x:xs) | not (esPosicionGanadora(jugar p x)) = todasLasGanadoras p xs + 1
                           | otherwise = todasLasGanadoras p xs






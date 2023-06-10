import Text.Show.Functions()

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

bart,todd,rafa :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m] --esta x entre n y m ?

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


-- 1 modelar los palos de golf que pueden usarse y los obstáculos 
type Palo = Habilidad -> Tiro
putter,madera :: Palo
putter habilidad  = UnTiro {velocidad = 10, precision = (precisionJugador habilidad)*2 , altura = 0}
madera habilidad = UnTiro {velocidad = 100, altura = 5 , precision = (precisionJugador habilidad)`div`2}
hierro :: Int -> Palo
hierro numero habilidad = UnTiro {velocidad = fuerzaJugador habilidad * numero , precision = precisionJugador habilidad `div` numero , altura = alturaConHierro numero}

alturaConHierro:: Int -> Int
alturaConHierro numero = maximum (0, numero-3)

palosDeHierro:: [Palo]
palosDeHierro = map (\n-> hierro n) [1..10]

palos :: [Palo]
palos = [putter, madera] ++ palosDeHierro

-- funcion golpe: dada una persona y un palo  genera un tiro

golpe:: Jugador-> Palo -> Tiro
golpe jugador palo  = (palo.habilidad) jugador


--




tiroDetenido::Tiro
tiroDetenido = UnTiro {velocidad = 0, precision = 0, altura = 0}
vaAlRasDelSuelo:: Tiro->Bool
vaAlRasDelSuelo = ( == 0).altura 





-- Con data se puede acceder por un lado a la condicion de superacion, y por el otro al efecto ("Refactor" de la funcion)
data Obstaculo = Obstaculo {
                              elTiroLoSupera :: (Tiro->Bool),
                              efectoSobreTiro :: (Tiro->Tiro)
                             }deriving(Show)

-- tunel --
tunel::Obstaculo
tunel = Obstaculo superaTunel efectoTunel

superaTunel:: Tiro -> Bool
superaTunel tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

efectoTunel:: Tiro -> Tiro
efectoTunel tiro 
  |superaTunel tiro = UnTiro{velocidad = velocidad tiro *2 , altura = 0 , precision = 100}
  |otherwise = tiroDetenido

--laguna--

laguna::Int -> Obstaculo
laguna largo = Obstaculo {elTiroLoSupera = superaLaguna , efectoSobreTiro = (efectoLaguna largo) }
  

superaLaguna:: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)


efectoLaguna:: Int-> Tiro -> Tiro
efectoLaguna largo tiro = tiro{ altura = altura tiro `div` largo} 

--hoyo

hoyo::Obstaculo
hoyo = Obstaculo superaHoyo efectoHoyo

superaHoyo:: Tiro ->Bool
superaHoyo tiro = between 5 20 (velocidad tiro) && vaAlRasDelSuelo tiro
efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido


------
palosUtiles:: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar obstaculo jugador) palos

leSirveParaSuperar:: Obstaculo -> Jugador -> Palo -> Bool
leSirveParaSuperar obstaculo jugador palo = elTiroLoSupera obstaculo (golpe jugador palo)




cuantosPuedeSuperar tiro [] = 0
cuantosPuedeSuperar tiro (obstaculo : obstaculos)
    |elTiroLoSupera obstaculo tiro = 1 +   (cuantosPuedeSuperar (efectoSobreTiro obstaculo tiro) obstaculos)
    |otherwise = 0






--maximoSegun f = foldl1 (mayorSegun f)

--mayorSegun f a b
  -- | f a > f b = a
 -- | otherwise = b
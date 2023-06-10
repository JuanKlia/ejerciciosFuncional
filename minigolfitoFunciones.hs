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

tiroDetenido = UnTiro {velocidad = 0, precision = 0, altura = 0}


--parametrizar las cosas que cambian o varian entre la logica repetida : la condicion de superacion, y el efecto que tiene sobre el tiro

obstaculo :: (Tiro->Bool)->(Tiro->Tiro)->Tiro  -> Tiro
obstaculo condicionDeSuperacion efectoSobreTiro tiroOriginal 
    |condicionDeSuperacion tiroOriginal = efectoSobreTiro tiroOriginal
    |otherwise = tiroDetenido

type Obstaculo = Tiro -> Tiro
-- una vez homogenizada la logica se puede reescribir los obstaculos en funcion de esta fx

-- tunel 

tunelConRampita, hoyo :: Obstaculo
laguna:: Int -> Obstaculo

tunelConRampita = obstaculo superaTunel efectoTunel  -- constante a funcion parcial que espera un tiro y retorna un tiro modificado

superaTunel:: Tiro -> Bool
superaTunel tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo:: Tiro->Bool
vaAlRasDelSuelo = ( == 0).altura 

efectoTunel:: Tiro -> Tiro
efectoTunel tiro = UnTiro{velocidad = velocidad tiro *2 , altura = 0 , precision = 100}

--laguna--


laguna largo = obstaculo superaLaguna (efectoLaguna largo)  

superaLaguna:: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

efectoLaguna:: Int-> Tiro -> Tiro
efectoLaguna largo tiro = UnTiro{ altura = altura tiro `div` largo} 

--hoyo


hoyo = obstaculo superaHoyo efectoHoyo

superaHoyo:: Tiro ->Bool
superaHoyo tiro = between 5 20 (velocidad tiro) && vaAlRasDelSuelo tiro
efectoHoyo :: Tiro -> Tiro
efectoHoyo tiro = tiroDetenido


------

-- palosUtiles jugador obstaculo = filter (\palo -> (golpe jugador palo) supera el obstaculo? no hay forma de acceder a las condiciones) palos


   

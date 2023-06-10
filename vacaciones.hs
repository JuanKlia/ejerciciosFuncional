import Text.Show.Functions
import Data.List

type Idioma = String

data Turista = Turista {cansancio::Int , stress :: Int, viajaSolo :: Bool , idiomas :: [Idioma]}deriving(Show)

ana,beto,cathi :: Turista

ana = Turista 0 21 False ["espaniol"]
beto = Turista 15 15 True ["aleman"]
cathi = Turista 15 15 True ["aleman","catalan"]

type Excursion = Turista -> Turista

irALaPlaya:: Excursion
irALaPlaya turista 
    |viajaSolo turista = reducirElStress $ modificarCansancio (-) 5 turista 
    |otherwise = reducirElStress $ modificarStress (-) 1 turista 

apreciarElementoDelPaisaje,salirAHablar:: String -> Excursion
apreciarElementoDelPaisaje elemento  =  reducirElStress.modificarStress (-) (cantidadDeLetras elemento) 

salirAHablar idioma  = reducirElStress.modificarCompania.aprenderIdioma idioma

caminar:: Int -> Excursion
caminar minutos  =  reducirElStress.modificarStress (-) (nivelDeIntensidad minutos).modificarCansancio (+) (nivelDeIntensidad minutos)
    where
        nivelDeIntensidad =  ( `div` 4)

paseoEnBarco:: String -> Excursion
paseoEnBarco "fuerte"=  reducirElStress.modificarCansancio (+)10.modificarStress (+) 6 
paseoEnBarco "moderado" = reducirElStress.id
paseoEnBarco "tranquilo" =  reducirElStress.salirAHablar "aleman".apreciarElementoDelPaisaje "mar" .caminar 10
paseoEnBarco otherwise = id

reducirElStress:: Turista -> Turista
reducirElStress turista = modificarStress (-) (elDiezPorciento $ stress turista) turista



elDiezPorciento numero = (numero * 10) `div` 100
cantidadDeLetras = genericLength
modificarStress operacion  cantidad turista = turista{ stress = operacion (stress turista) cantidad }
modificarCansancio operacion cantidad turista  = turista{ cansancio = operacion (stress turista) cantidad }
aprenderIdioma idioma turista = turista{idiomas = idioma : idiomas turista}
modificarCompania turista = turista  {viajaSolo = False}


-----------------------------------------------------------------------------------------------------

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun:: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (excursion turista) turista

esExcursionEducativa::  Turista -> Excursion -> Bool
esExcursionEducativa turista   = ( >0).deltaExcursionSegun (numero.idiomas) turista 
        where
            numero = genericLength

excursionesDesetresantes:: Turista -> [Excursion] -> [Excursion]
excursionesDesetresantes turista  = filter (esDesestresante turista) 

esDesestresante:: Turista -> Excursion -> Bool
esDesestresante turista  = ( <= (-3) ).deltaExcursionSegun stress turista 

------------------------------------------------------------------------------------------------------

type Tour = [Excursion]
completo:: Tour 
completo = [caminar 20 , apreciarElementoDelPaisaje "cascada", irALaPlaya , salirAHablar "melmacquiano"]

ladoB:: Excursion -> Tour
ladoB excursionElegida = [paseoEnBarco "tranquilo", excursionElegida, caminar 120]

islaVecina:: String -> Tour
islaVecina marea = [paseoEnBarco marea , excursionSegun marea ,paseoEnBarco marea]

excursionSegun "fuerte" = apreciarElementoDelPaisaje "lago"
excursionSegun otherwise = irALaPlaya



realizarTour:: Turista -> Tour -> Turista
realizarTour turista tour  =  realizarExcursiones tour $ aumentarStress turista tour 
    where
        cantidadExcursiones = genericLength
        aumentarStress turista tour = modificarStress (+) (cantidadExcursiones tour) turista

realizarExcursiones:: Tour -> Turista -> Turista
realizarExcursiones tour turista = foldl realizarExcursion turista tour
    where
        realizarExcursion turista excursion = excursion turista


type ConjuntoTours = [Tour]   --[ [viaje, playa]  [....] ]

hayTourConvincente :: ConjuntoTours -> Turista -> Bool
hayTourConvincente tours turista = any (esConvincentePara turista) tours  -- hay tour convincente? 

-- Un tour es convincente cuando tiene alguna excursion desestresante y deja al usuario acompañado
esConvincentePara:: Turista -> Tour -> Bool
esConvincentePara turista   = any (estaAcompaniado turista).(excursionesDesetresantes turista)

estaAcompaniado:: Turista -> Excursion -> Bool
estaAcompaniado turista = (False==).viajaSolo.( $ turista)



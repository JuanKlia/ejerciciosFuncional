import Data.List
import Text.Show.Functions


data Juguete = Juguete{                                         
    nombre ::String,
    duenio :: String,
    facha :: Float,
    accesorios :: [Accesorio],
    estaVivo :: Bool
}deriving(Show)

data Accesorio = Accesorio {
    efecto :: Efecto,
    eficacia :: Float
}deriving(Show)



--- Ejercicio 1


type Efecto = Float -> Juguete -> Juguete

lucirAmenzante, masSteel, vieneAndy :: Efecto

lucirAmenzante eficacia  = modificarFacha (10 + eficacia) 
masSteel eficacia juguete = modificarNombre "Max Steel" $ modificarFacha (eficacia* cantidadDeLetras (nombre juguete)) juguete
vieneAndy _ juguete = juguete {estaVivo = False} 

quemadura:: Float -> Efecto
quemadura grado eficacia juguete = modificarFacha (negate $ (-) (facha juguete) (eficacia + 2) * grado) juguete


modificarFacha:: Float -> Juguete -> Juguete
modificarFacha cambio juguete = juguete { facha = (facha juguete) + cambio}

modificarNombre:: String -> Juguete -> Juguete
modificarNombre cambio juguete = juguete {nombre = cambio}


--- Ejercicio 2
serpienteEnBota, radio, revolver, escopeta, lanzaLlamas :: Accesorio

serpienteEnBota = Accesorio {efecto = lucirAmenzante , eficacia = 2}
radio = Accesorio {efecto = vieneAndy, eficacia = 3}
revolver = Accesorio {efecto = masSteel , eficacia = 5}
escopeta = Accesorio {efecto = masSteel, eficacia = 20}
lanzaLlamas = Accesorio {efecto = quemadura 3  , eficacia = 8.5}  


--- Ejercicio 3
woody,soldado,barbie :: Juguete
woody = Juguete {
    nombre = "woody",
    duenio = "andy",
    estaVivo = True,
    facha = 100,
    accesorios = [serpienteEnBota, revolver]
}
soldado = Juguete {
    nombre = "soldado",
    duenio = "andy",
    estaVivo = True,
    facha = 5,
    accesorios = [lanzaLlamas, radio]
}
barbie = Juguete {
    nombre = "barbie",
    duenio = "dany",
    estaVivo = False,
    facha = 95.5,
    accesorios = [lanzaLlamas, escopeta,revolver]
}

--- Ejercicio 4

esImpaktante:: Juguete->Bool
esImpaktante juguete = any ((>10).eficacia) (accesorios juguete)


--- Ejercicio 5
esDislexico:: Juguete -> Bool
esDislexico =  (evaluarDislexiaCon "andy").duenio

evaluarDislexiaCon:: String -> String -> Bool
evaluarDislexiaCon palabra nombreDuenio = (nombreDuenio /= palabra) &&(cantidadDeLetras nombreDuenio == cantidadDeLetras palabra) && (tieneLasLetrasDe palabra nombreDuenio)

tieneLasLetrasDe palaba nombreDuenio = all (\letra -> elem letra palaba) nombreDuenio

cantidadDeLetras = genericLength

     


--- Ejercicio 6

type Cajon = [Juguete]
cajon = [woody, soldado, barbie]

cuantosJuguetes:: (Juguete->Bool) -> (Cajon->Int)
cuantosJuguetes criterio  = cuantos.filter criterio 
    where
        cuantos = genericLength

cuantosImpaktantes,cuantosNombreLargo,cuantosDislexicosYNoVivos :: (Cajon->Int)
cuantosImpaktantes = cuantosJuguetes esImpaktante
cuantosNombreLargo = cuantosJuguetes tienenNombreLargo
cuantosDislexicosYNoVivos = cuantosJuguetes sonDislexicosYNoVivos 



tienenNombreLargo = (>6).longitud.nombre
    where
        longitud= genericLength

sonDislexicosYNoVivos juguete =  esDislexico juguete && not(estaVivo juguete)





--- Ejercicio 7

nivelDeAmor:: Juguete -> Float
nivelDeAmor = calcularAmor.aplicarTodosLosAccesorios 


aplicarTodosLosAccesorios:: Juguete -> Juguete
aplicarTodosLosAccesorios juguete = foldl (flip aplicarAccesorio) juguete (accesorios juguete)

aplicarAccesorio::  Accesorio -> Juguete -> Juguete

aplicarAccesorio accesorio  = (aplicarEficacia accesorio $)

aplicarEficacia:: Accesorio -> (Juguete -> Juguete)
aplicarEficacia accesorio = efecto accesorio $ eficacia accesorio

calcularAmor:: Juguete -> Float
calcularAmor juguete
    |estaVivo juguete = (2*).amor $ juguete
    |otherwise = amor juguete
    where
        amor juguete = facha juguete + cantidadDeLetras (nombre juguete) * 5 - cantidad (accesorios juguete) * 7 
        cantidad = genericLength




--- Ejercicio 8

pasaMontanias::Accesorio
pasaMontanias = Accesorio {efecto = lucirAmenzante , eficacia = 7}

nivelDeAmorDelCajon:: Cajon -> Float
nivelDeAmorDelCajon =   sumatoriaDeAmor.(agregarAccesorioATodos pasaMontanias)

agregarAccesorioATodos:: Accesorio -> Cajon -> Cajon
agregarAccesorioATodos accesorio  = map (agregarAccesorio accesorio) 

agregarAccesorio:: Accesorio -> Juguete -> Juguete
agregarAccesorio accesorio juguete = juguete {accesorios = accesorio : (accesorios juguete)}


sumatoriaDeAmor:: Cajon -> Float
sumatoriaDeAmor  = foldl (\acumulador juguete -> acumulador + nivelDeAmor juguete) 0 

import Text.Show.Functions
import Data.List
                                   
data Persona = Persona {
    nombre :: String,
    edad :: Int,
    felicidonios :: Int,
    suenios :: [Suenio],
    habilidades :: [String]     
}deriving(Show)

type Suenio = Persona->Persona


juan :: Persona
juan = Persona { nombre = "Juan", 
                 edad = 25, 
                 felicidonios = 101, 
                 suenios = [recibirseDe "medicina", viajarA ["Barcelona"]],
                 habilidades = ["Correr"]
                 }

maria :: Persona
maria = Persona { nombre = "Maria", 
                  edad = 26, 
                  felicidonios = 100, 
                  suenios = [recibirseDe "arquitecta", enamorarseDe mateo],
                  habilidades = ["Saltar"]
                 }
maximiliano :: Persona
maximiliano = Persona { nombre = "Maximiliano", 
                        edad = 29, 
                        felicidonios = 50, 
                        suenios = [viajarA ["Londres"]],
                        habilidades = ["Jugar al futbol"]
                 }
 
------------------------- PUNTO 1 -------------------------

coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona
    | esMuyFeliz persona = felicidonios persona * edad persona
    | esFeliz persona = cantidadDeSuenios persona * felicidonios persona
    | otherwise =  felicidonios persona `div` 2


gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    | esMuyFeliz persona = felicidonios persona * cantidadDeSuenios persona
    | esFeliz persona = edad persona * cantidadDeSuenios persona
    | otherwise =  (2*) $ cantidadDeSuenios persona 

-- FUNCIONES AUXILIARES:                                  
esMuyFeliz :: Persona -> Bool
esMuyFeliz persona = felicidonios persona > 100

esFeliz :: Persona -> Bool
esFeliz persona = felicidonios persona > 50 && felicidonios persona <= 100

cantidadDeSuenios :: Persona -> Int
cantidadDeSuenios = genericLength.suenios


------------------------- PUNTO 2 -------------------------

esNombreLargo :: Persona -> Bool
esNombreLargo =  (>10) . genericLength . nombre

personaSuertuda :: Persona -> Bool
personaSuertuda =  even . (*3) . coeficienteDeSatisfaccion

tieneNombreLindo :: Persona -> Bool
tieneNombreLindo = (=='a') . last . nombre



------------------------- PUNTO 3 -------------------------

recibirseDe :: String -> (Persona -> Persona)
recibirseDe carrera persona = persona{
                                habilidades = carrera : habilidades persona,
                                felicidonios = aumentarFelicidonios (1000*(longitud carrera)) persona
                              }

viajarA :: [String] -> Persona -> Persona
viajarA ciudadesVisitadas persona = persona{   
                                  edad = edad persona + cantidad ciudadesVisitadas,
                                  felicidonios =  aumentarFelicidonios (100 * (cantidad ciudadesVisitadas)) persona
                               }

enamorarseDe :: Persona -> Persona -> Persona
enamorarseDe personaDeLaQueSeEnamora persona = persona{   
                                   felicidonios = aumentarFelicidonios (felicidonios personaDeLaQueSeEnamora) persona
                               }


todoSigueIgual :: Persona -> Persona
todoSigueIgual = id


comboPerfecto :: Persona -> Persona
comboPerfecto persona  = viajarA["Berazategui","Paris"].(recibirseDe "Medicina") $ persona { felicidonios = aumentarFelicidonios 100 persona}


-- FUNCIONES AUXILIARES:
aumentarFelicidonios:: Int -> Persona -> Int
aumentarFelicidonios aumento persona =   aumento + felicidonios persona 

longitud :: String->Int
longitud = genericLength

cantidad :: [String]->Int
cantidad = genericLength







--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                     --xxxxxxxxxxxxxxxxxxxxxxxxx Segunda Parte xxxxxxxxxxxxxxxxxxxxxxxxx--
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
eugenia::Persona
eugenia = Persona{ nombre = "Eugenia",
                   edad = 22,
                   felicidonios = 5000,
                   suenios = [recibirseDe "disenoDeInteriores", viajarA ["Paris"], enamorarseDe manuel],
                   habilidades = []
}
manuel :: Persona
manuel = Persona {
                   nombre = "Manuel",
                   edad = 22,
                   felicidonios = 15,
                   suenios = [],
                   habilidades = []
                 }

martina::Persona
martina = Persona{ nombre = "Martina",
                   edad = 22,
                   felicidonios = 500,
                   suenios = [comboPerfecto, recibirseDe "medicina", enamorarseDe mateo, viajarA ["Barcelona"]],
                   habilidades = []

               }   
agustin::Persona
agustin = Persona{ nombre = "Agustin",
                   edad = 22,
                   felicidonios = 100,
                   suenios = [todoSigueIgual],
                   habilidades = []

               }   
mateo::Persona
mateo = Persona{ nombre = "Mateo",
                   edad = 22,
                   felicidonios = 0,
                   suenios = [],
                   habilidades = []

               }   



------------------------- PUNTO 4 -------------------------

type Fuente = Persona->Persona

fuenteMinimalista:: Fuente     --hacer wheres
fuenteMinimalista persona  = ((head.suenios) persona  persona ) { suenios = tail (suenios persona)}

fuenteCopada::Fuente
fuenteCopada persona =  foldl (\ personaAcumulada _ ->  fuenteMinimalista personaAcumulada) persona (suenios persona) 

fuenteAPedido:: Int -> Fuente
fuenteAPedido enesimo persona  = ( (suenios persona) !! (enesimo-1)) persona   

fuenteSorda:: Fuente
fuenteSorda = id

------------------------- PUNTO 5 -------------------------


type Criterio = Persona -> Fuente -> Fuente -> Fuente

mejorFuenteSegun:: Criterio -> Persona ->[Fuente] -> Fuente
mejorFuenteSegun funcionCriterio persona fuentes = foldl1 (funcionCriterio persona) fuentes 

criterio persona operador funcion  fuente1 fuente2
    |operador (funcion.fuente1 $ persona) (funcion.fuente2 $ persona) = fuente1
    |otherwise = fuente2

masFelicidonios,menosFelicidonios,masHabilidades :: Criterio
masFelicidonios persona = criterio persona (>) felicidonios   
menosFelicidonios persona = criterio persona (<) felicidonios
masHabilidades persona = criterio persona  (>) numeroDeHabilidades

numeroDeHabilidades:: Persona-> Int
numeroDeHabilidades  = genericLength.habilidades 




--- Esta logica salio del intento de refactorizar la siguiente logica repetida:

-- masFelicidonios:: Criterio
-- masFelicidonios persona fuente1 fuente2  
--      | felicidoniosPrimerFuente persona > felicidoniosSegundaFuente persona = fuente1
--      |otherwise = fuente2
--      where
--        felicidoniosPrimerFuente =  felicidonios.fuente1 
--        felicidoniosSegundaFuente = felicidonios.fuente2

-- menosFelicidonios:: Criterio
-- menosFelicidonios persona fuente1 fuente2  
--        | felicidoniosPrimerFuente persona < felicidoniosSegundaFuente persona = fuente1
--        |otherwise = fuente2
--        where
--            felicidoniosPrimerFuente =  felicidonios.fuente1 
--            felicidoniosSegundaFuente = felicidonios.fuente2
--
-- masHabilidades:: Criterio
-- masHabilidades persona fuente1 fuente2  
--        |numeroHabilidadesPrimerFuente persona > numeroHabilidadesSegundaFuente persona = fuente1
--        |otherwise = fuente2
--        where
--            numeroHabilidadesPrimerFuente =  genericLength.habilidades.fuente1 
--            numeroHabilidadesSegundaFuente = genericLength.habilidades.fuente2





------------------------- PUNTO 6 ------------------------- solo fx ord sup y app parcial

sueniosValiosos:: Persona -> [Suenio]
sueniosValiosos persona = filter ((>100).felicidonios.( $ persona)) (suenios persona)


tieneSuenioRarop :: Persona -> Bool
tieneSuenioRarop persona = any ( (felicidonios persona ==).felicidonios.(  $ persona) ) (suenios persona)



listaDePersonas :: [Persona]
listaDePersonas = [martina,agustin]

felicidoniosTotales :: [Persona] -> Int
felicidoniosTotales listaPersonas = foldl (+) 0 (map   ( felicidonios.(fuenteCopada $ ) )  listaDePersonas )


------------------------- PUNTO 7 -------------------------

soniadorCronico :: Persona            
soniadorCronico = Persona {
    edad = 22,
    suenios = cycle [enamorarseDe martina, viajarA ["Malaga"], recibirseDe "ingenieriaEnSistemas"] ,
    nombre = "Soniador Cronico",
    felicidonios = 300,
    habilidades = ["cantar"]
} 

--Este ejercicio se relaciona con el concepto de evaluacion diferida
-- Soniador cronico con infinitos suenios de recibirse ingresa a FuenteMinimalista : la operacion converge ya que es un head de una lista infinita, solo le interesa generar el primer elemento
-- Soniador cronico con infinitos suenios de recibirse ingresa a FuenteCopada : la operacion no converge, el foldl nunca termina de acumular
-- Soniador cronico con infinitos suenios de recibirse ingresa a FuenteAPedido : la operacion converge, ya que genera hasta el suenio enesimo y lo cumple
-- Soniador cronico con infinitos suenios de recibirse ingresa a FuenteSorda: la operacion converge ya que es la funcion ID

-- Los funciones que convergen logran realizar la operacion, pero a la hora de mostrar por consola a la persona modificada llega hasta el campo suenio, donde se queda iterando al infinito

-----------------------------------------------------------------------------------------------------------------
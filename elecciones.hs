import Data.List
import Text.Show.Functions

data Candidato = Candidato{
    nombre :: String,
    edad :: Double,
    carisma :: Double,
    capacidadesParaConvencer:: [Capacidad]
}deriving(Show)

type Capacidad = Candidato -> Double 

facha,liderazgo,riqueza,corrupto,tiktoker,flogger :: Capacidad
facha candidato = (60 - edad candidato)  + carisma candidato * 3
liderazgo candidato = edad candidato * 10
riqueza candidato = carisma candidato + (edad candidato / 50 )
corrupto _ = -100
tiktoker _ = 100
flogger _ = 0

cintia,marcos :: Candidato
cintia = Candidato "cintia" 40 12 [liderazgo,riqueza ,tiktoker]
marcos = Candidato "marcos"  45 10 [facha,liderazgo, corrupto]


tieneCapacidadInutil:: Candidato -> Bool
tieneCapacidadInutil candidato = any (esCapacidadInutilPara candidato) (capacidadesParaConvencer candidato)
esCapacidadInutilPara:: Candidato -> Capacidad -> Bool
esCapacidadInutilPara candidato =  (<=0).( $ candidato)

--

losQueSoloTienenCapacidadesUtiles:: [Candidato] -> [Candidato]
losQueSoloTienenCapacidadesUtiles candidatos = filter (not.tieneCapacidadInutil) candidatos 

--
nombreYConvencimientoTotal::Candidato -> (String, Double)
nombreYConvencimientoTotal candidato = (nombre candidato , convencimientoTotalDe candidato)

convencimientoTotalDe:: Candidato -> Double
convencimientoTotalDe  = max 0 . sumatoriaDeConvencimiento
    where
        sumatoriaDeConvencimiento candidato = sum $ map ( $ candidato) (capacidadesParaConvencer candidato)
--



--
type Nombre = String
type Votos = Double
type VotosDe = (Nombre, Votos)

votacion :: Double -> [Candidato] -> [VotosDe]
votacion numeroVotantes candidatos = map (resultadoVotacionCandidato (totalConvencimientoDeTodos candidatos) numeroVotantes) candidatos


resultadoVotacionCandidato:: Double -> Double -> Candidato -> VotosDe
resultadoVotacionCandidato convencimientoTotal numeroVotantes candidato = (nombre candidato , cantidadDeVotos numeroVotantes convencimientoTotal candidato)

totalConvencimientoDeTodos:: [Candidato] -> Double
totalConvencimientoDeTodos = sum.map convencimientoTotalDe 

cantidadDeVotos:: Double -> Double -> Candidato -> Double
cantidadDeVotos  numeroVotantes totalConvencimiento candidato = (numeroVotantes * (convencimientoTotalDe candidato)) / totalConvencimiento




-- de un conjunto de candidatos


elCandidato criterio candidatos = foldl1 (elMejor3 criterio) candidatos

masMolestoDeNombrar:: [Candidato]->Candidato
masMolestoDeNombrar  = elCandidato nombreMasLargo 
    where
        nombreMasLargo = genericLength.nombre

ganador:: Double -> [Candidato] -> Candidato
ganador numeroVotantes candidatos = elCandidato (conMasVotos numeroVotantes candidatos) candidatos
    where
        conMasVotos numeroVotantes  = cantidadDeVotos numeroVotantes. totalConvencimientoDeTodos 

masFachero:: [Candidato]-> Candidato
masFachero = elCandidato facha


elMejor3:: Ord b => (a->b)-> a -> a-> a
elMejor3 operacion elem1 elem2 
    |operacion elem1 >= operacion elem2 = elem1
    |otherwise = elem2



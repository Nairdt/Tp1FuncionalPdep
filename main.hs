import Text.Show.Functions
import Data.Char

--3.1

data Auto = Auto {nombre :: String, nivelDeNafta :: Float, velocidad :: Float, nombreEnamorade :: String, truco :: String} deriving (Show)
type FuncionAuto = Auto -> Auto

deReversa :: FuncionAuto
deReversa unAuto = unAuto {nivelDeNafta = ((+200).nivelDeNafta) unAuto} 

impresionar :: FuncionAuto
impresionar unAuto = unAuto {velocidad = ((2*).velocidad) unAuto}

nitro :: FuncionAuto
nitro unAuto = unAuto {velocidad = ((15+).velocidad) unAuto}

fingirAmor :: Auto -> String -> Auto
fingirAmor unAuto unNombre= unAuto {nombreEnamorade = unNombre}

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" "deReversa"
biankerr = Auto "Biankerr" 500 20 "Tinch" "impresionar"
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" "nitro"
rodra = Auto "Rodra" 0 50 "Taisa" "fingirAmor"

-- 3.2
incrementarVelocidad :: Auto-> Auto
incrementarVelocidad unAuto 
  | cantidadDeVocalesEnamorade unAuto >=1  && cantidadDeVocalesEnamorade unAuto <= 2 = unAuto{velocidad = ((+15).velocidad) unAuto} 
  | cantidadDeVocalesEnamorade unAuto >= 3 && cantidadDeVocalesEnamorade unAuto <= 4 = unAuto{velocidad = ((+20).velocidad) unAuto}
  | cantidadDeVocalesEnamorade unAuto >= 4 = unAuto{velocidad = ((+30).velocidad) unAuto}
  | otherwise = unAuto
cantidadDeVocalesEnamorade :: Auto -> Int
cantidadDeVocalesEnamorade unAuto = cantidadDeA (nombreEnamorade unAuto) + cantidadDeE (nombreEnamorade unAuto) + cantidadDeI (nombreEnamorade unAuto) + cantidadDeO (nombreEnamorade unAuto) + cantidadDeU (nombreEnamorade unAuto)

cantidadDeA :: String -> Int
cantidadDeA  = length.(filter (== 'a')).pasarAMinuscula
cantidadDeE :: String -> Int
cantidadDeE  = length.(filter (== 'e')).pasarAMinuscula
cantidadDeI :: String -> Int
cantidadDeI  = length.(filter (== 'i')).pasarAMinuscula
cantidadDeO :: String -> Int
cantidadDeO  = length.(filter (== 'o')).pasarAMinuscula
cantidadDeU :: String -> Int
cantidadDeU  = length.(filter (== 'u')).pasarAMinuscula

pasarAMinuscula :: String -> String
pasarAMinuscula  = map toLower 

-- 3.3

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = nivelDeNafta unAuto > 0 && velocidad unAuto < 100

-- 3.4

comboLoco :: FuncionAuto
comboLoco = deReversa.nitro

queTrucazo :: Auto -> String -> Auto
queTrucazo unAuto unNombre = incrementarVelocidad (fingirAmor unAuto unNombre)

turbo :: FuncionAuto
turbo = vaciarNafta.aumentarVelocidadPorNivelDeNafta

vaciarNafta :: FuncionAuto
vaciarNafta unAuto = unAuto{nivelDeNafta = 0}

aumentarVelocidadPorNivelDeNafta :: FuncionAuto
aumentarVelocidadPorNivelDeNafta unAuto = unAuto{velocidad= velocidad unAuto + ((nivelDeNafta unAuto)*10)}


--Casos De Prueba
--3.1
--deReversa rochaMcQueen
--impresionar biankerr
--nitro gushtav
--fingirAmor rodra "Petra"
--3.2
--incrementarVelocidad rochaMcQueen
--incrementarVelocidad biankerr
--incrementarVelocidad gushtav
--incrementarVelocidad rodra 
--3.3
--puedeRealizarTruco rochaMcQueen
--puedeRealizarTruco gushtav
--puedeRealizarTruco rodra
--3.4
--comboLoco rochaMcQueen
--queTrucazo rodra "Murcielago"
--turbo gushtav
--turbo rodra
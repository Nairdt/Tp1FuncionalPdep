import Text.Show.Functions
import Data.Char
data Auto = Auto {nombre :: String, nivelDeNafta :: Float, velocidad :: Float, nombreEnamorade :: String, truco :: Truco} deriving (Show)
type Truco = Auto -> Auto

rayoMcQueen = Auto " " 1.0 5 "unAuto" deReversa

deReversa :: Truco
deReversa unAuto = unAuto {nivelDeNafta = ((+200	).nivelDeNafta) unAuto} 

impresionar :: Truco
impresionar unAuto = unAuto {velocidad = ((2*).velocidad) unAuto}

nitro :: Truco
nitro unAuto = unAuto {velocidad = ((15+).velocidad) unAuto}

fingirAmor :: Auto -> Auto
fingirAmor unAuto = unAuto

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" deReversa
biankerr = Auto "Biankerr" 500 20 "Tinch" impresionar
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" nitro
rodra = Auto "Rodra" 0 50 "Taisa" fingirAmor

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
puedeRealizarTruco unAuto = nivelDeNafta unAuto > 0 && velocidad unAuto > 100

-- 3.4

comboLoco :: Truco
comboLoco = deReversa.nitro

queTrucazo :: Truco
queTrucazo = incrementarVelocidad.fingirAmor

turbo :: Truco
turbo = vaciarNafta.aumentarVelocidadPorNivelDeNafta

vaciarNafta :: Truco
vaciarNafta unAuto = unAuto{nivelDeNafta = 0}

aumentarVelocidadPorNivelDeNafta :: Truco
aumentarVelocidadPorNivelDeNafta unAuto = unAuto{velocidad= velocidad unAuto + ((nivelDeNafta unAuto)*10)}
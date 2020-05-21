module Lib where

data TipoPokemon = Planta | Agua | Fuego deriving (Show, Eq)

tieneVentajaContra Planta Agua = True
tieneVentajaContra Agua Fuego = True
tieneVentajaContra Fuego Planta = True
tieneVentajaContra _ _ = False



-- Punto 1: Se quiere conocer a qué pokemones les puede ganar un pokemon dado (es decir, a cuáles aventaja por su tipo). Pensar: ¿Qué cosas necesito recibir? ¿y qué devolver?

data Pokemon = UnPokemon { 
  nombre :: String, 
  tipo :: TipoPokemon
} deriving (Show, Eq)

aQuienesLesGana :: Pokemon -> [Pokemon] -> [Pokemon]
aQuienesLesGana atacante contrincantes = filter (leGana atacante) contrincantes

--(leGana atacante) :: Pokemon -> Bool

leGana :: Pokemon -> Pokemon -> Bool
leGana pokeAtacante pokeDefensor = tieneVentajaContra (tipo pokeAtacante) (tipo pokeDefensor)

charmander = UnPokemon "Charmander" Fuego
flareon = UnPokemon "Flareon" Fuego
gyarados = UnPokemon "Gyarados" Agua
carpinchos = UnPokemon "Carpinchos" Agua

-- Punto 2 : Se quiere conocer los nombres de los pokemones del punto anterior.

nombresPerdedores :: Pokemon -> [Pokemon] -> [String]
nombresPerdedores atacante contrincantes = map nombre (aQuienesLesGana atacante contrincantes)

nombresPerdedoresV2 atacante = map nombre . aQuienesLesGana atacante

-- Punto 3 : Teniendo un pokemon, se quiere conocer a cuántos puede ganarle de una lista de pokemones.

cantidadVictorias :: Pokemon -> [Pokemon] -> Int
cantidadVictorias atacante contrincantes = length (aQuienesLesGana atacante contrincantes)

{- Punto 4
Se sabe que un destino a donde puede pelear un pokemon puede ser un gimnasio o una liga. Los gimnasios son consecutivos (se sabe cuál es el siguiente de cada uno) y al final de un camino siempre hay una liga. Por ahora sólo nos interesan los pokemones contrincantes que existen en una liga.
Dada la siguiente definición -}
data Destino = UnGimnasio { nombreGym:: String, siguiente:: Destino }
              | UnaLiga { contrincantes:: [Pokemon] } deriving (Show, Eq)
-- Se desea saber si un pokemon está al horno en un destino. En un gimnasio, un pokemon siempre está al horno, y en una liga, está al horno cuando todos los contrincantes pueden ganarle. 

gymRoca :: Destino
gymRoca = UnGimnasio "Gimnasio Roca" gymAgua
gymAgua :: Destino
gymAgua = UnGimnasio "Gimnasio Agua" gymElectrico
gymElectrico :: Destino
gymElectrico = UnGimnasio "Gimnasio Electrico" ligaKanto
ligaKanto :: Destino
ligaKanto = UnaLiga [flareon, gyarados, charmander]
gymFuego :: Destino
gymFuego = UnGimnasio "Gimnasio Fuego" gymPlanta
gymPlanta :: Destino
gymPlanta = UnGimnasio "Gimnasio Planta" ligaGali
ligaGali :: Destino
ligaGali = UnaLiga [carpinchos, gyarados]

-- Se desea saber si un pokemon está al horno en un destino. 
-- en una liga, está al horno cuando todos los contrincantes pueden ganarle. 
estaAlHorno :: Pokemon -> Destino -> Bool
estaAlHorno _ (UnGimnasio _ _) = True
estaAlHorno pokemon (UnaLiga contrincantes) = all (`leGana` pokemon) contrincantes

-- Esto está mal: all (leGana pokemon) contrincantes
-- Porque pokemon debería ser el segundo parámetro de leGana

estaAlHornoV1 pokemon (UnaLiga contrs) = all (pierdeContra pokemon) contrs
pierdeContra defensor atacante = leGana atacante defensor

estaAlHornoV2 pokemon (UnaLiga contrincantes) = all (`leGana` pokemon) contrincantes

estaAlHornoV3 poke (UnaLiga contrincantes) = all (\contrincante -> leGana contrincante poke) contrincantes

-- Esto no vamos a hacerlo, porque es más imperativo:
estaAlHornoV4 poke (UnaLiga contrs) = cantidadVictorias poke contrs == 0

esPar = (\nro -> mod nro 2 == 0)

-- punto 5
{-Saber si puedo viajar de un destino al otro. 
Consideraciones a tener en cuenta:
Desde una Liga no puedo viajar a otro destino.
Desde unGimnasio puedo viajar a miDestino si miDestino se encuentra entre los siguientes destinos de unGimnasio. Es decir, miDestino debe estar en el camino a seguir de unGimnasio.
-}

puedeViajar :: Destino -> Destino -> Bool

-- esto es un poquito imperativo, y tiene un pequeño problema de lógica.
puedeViajarV1 (UnaLiga _) _ = False
puedeViajarV1 origen destino | origen == destino = True
                           | otherwise = puedeViajar (siguiente origen) destino

-- esto es más declarativo, pero tiene un pequeño problema de lógica (siempre da falso)
puedeViajarV2 (UnaLiga _) _ = False
puedeViajarV2 origen destino =  origen == destino || puedeViajar (siguiente origen) destino

-- soluciona el problema de lógica.
puedeViajarV3 (UnaLiga contrincantes) destino = (UnaLiga contrincantes) == destino
puedeViajarV3 origen destino =  origen == destino || puedeViajar (siguiente origen) destino

-- soluciona el problema pero con un chiche que pueden googlear: "at pattern"
puedeViajar liga@(UnaLiga _) destino = liga == destino
puedeViajar origen destino =  origen == destino || puedeViajar (siguiente origen) destino

-- Punto 6
-- Conocer el pokemon que a más pokemones les puede ganar de una lista.

elMasPicante :: [Pokemon] -> Pokemon
elMasPicante pokemones = foldl1 (elMejorDeDos pokemones) pokemones

elMejorDeDos :: [Pokemon] -> Pokemon -> Pokemon -> Pokemon
elMejorDeDos pokemones poke1 poke2 | cantidadVictorias poke1 pokemones >= cantidadVictorias poke2 pokemones = poke1
                         | otherwise = poke2

-- no acepta listas vacias (y está bien)
miHead (x:xs) = x

-- no acepta listas vacias (y está bien)
-- foldl1 operacion (x:xs) = foldl operacion x xs

-- no acepta listas vacías y está mal, porque sum debería
sum' lista = foldl1 (+) lista

-- Este sum es la q va
sum'' lista = foldl (+) 0 lista
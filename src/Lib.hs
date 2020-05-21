module Lib where

data TipoPokemon = Planta | Agua | Fuego deriving (Show, Eq)

tieneVentajaContra Planta Agua = True
tieneVentajaContra Agua Fuego = True
tieneVentajaContra Fuego Planta = True
tieneVentajaContra _ _ = False


-- data Destino = UnGimnasio { nombre:: String, siguiente:: Destino }
--                | UnaLiga { contrincantes:: [Pokemon] }

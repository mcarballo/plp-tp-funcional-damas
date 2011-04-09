module Damas where

import Tablero

data Juego = J Color Tablero
data Movimiento = M Posicion Direccion
  deriving Show

data Arbol a = Nodo a [Arbol a] deriving Show
type ArbolJugadas = Arbol ([Movimiento], Juego)

type Valuacion = Juego -> Double

---- Funciones de regalo ----

instance Show Juego where
  show (J turno tablero) = "\n--Juegan las " ++ show turno ++ "s--\n" ++ show tablero

instance Eq Juego where
  j1 == j2 = tablero j1 == tablero j2 && color j1 == color j2

arbolDeJugadas :: Juego -> ArbolJugadas
arbolDeJugadas j = Nodo ([], j) $ zipWith agmov movs hijos
  where agmov m (Nodo (ms, r) hs) = Nodo ((m:ms), r) (map (agmov m) hs)
        movsJuegos = movimientosPosibles j
        movs = map fst movsJuegos
        hijos = map (arbolDeJugadas . snd) movsJuegos

---- Ejercicios ----

-- Ejercicio 3
-- mover :: Movimiento -> Juego -> Maybe Juego
mover :: Movimiento -> Juego -> Maybe Juego
mover mov jue = Nothing

-- Ejercicio 4
movimientosPosibles :: Juego -> [(Movimiento, Juego)]
movimientosPosibles = error "falta implementar"

-- Ejercicio 5
-- foldArbol :: ...

-- Ejercicio 6
-- podar :: Int -> Arbol a -> Arbol a

-- Ejercicio 7
-- minimax :: Valuacion -> ArbolJugadas -> (Double, [Movimiento])

-- Ejercicio 8
-- ganador :: Juego -> Maybe Color

-- Ejercicio 9
-- valuacionDamas :: Juego -> Double




------FUNCIONES PARA TESTS----------

posicionesSinFichas juego = [pos|pos<-posicionesValidas,contenido pos (tablero juego) == Nothing]
posicionesConFichasDeColor juego color = posicionesSimplesDeColor juego color ++ posicionesReinasDeColor juego color

posicionesSimplesDeColor juego color = [pos | pos<-posicionesValidas, contenido pos (tablero juego) == (Just(Simple color))]

posicionesReinasDeColor juego color = [pos | pos<-posicionesValidas, contenido pos (tablero juego) == (Just(Reina color))]




tablero::Juego -> Tablero
tablero (J col t) = t

color::Juego -> Color
color (J col t) = col


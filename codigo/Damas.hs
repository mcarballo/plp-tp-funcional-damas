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

arbolDeJugadas :: Juego -> ArbolJugadas
arbolDeJugadas j = Nodo ([], j) $ zipWith agmov movs hijos
  where agmov m (Nodo (ms, r) hs) = Nodo ((m:ms), r) (map (agmov m) hs)
        movsJuegos = movimientosPosibles j
        movs = map fst movsJuegos
        hijos = map (arbolDeJugadas . snd) movsJuegos

---- Ejercicios ----

-- Ejercicio 3
-- mover :: Movimiento -> Juego -> Maybe Juego

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

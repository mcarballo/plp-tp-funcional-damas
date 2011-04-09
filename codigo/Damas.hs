module Damas where

import Tablero

----------------------- INCLUIDAS POR NOSOTROS---------------------------

import Char

-------------------------------------------------------------------------

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

mover :: Movimiento -> Juego -> Maybe Juego
mover mov jue = Nothing
{-
mover :: Movimiento -> Juego -> Maybe Juego
mover (M pos dir) (J col (T f)) = if cumpleCondiciones then hacerMovimiento else Nothing
				where
					cumpleCondiciones = posicionesEnRango && hayFichaEnOrigen && 									    mueveElJugadorCorrespondiente && mueveEnDirCorrecta &&
							    puedeMover  
					posicionOrigenEnRango =  enTablero pos
					posicionDestinoEnRango = if posLlegada == Nothing then False else enTablero posLlegada 
					noHayFichaEnDestino = 
					hayFichaEnOrigen = 
					mueveElJugadorCorrespondiente = 
					mueveEnDirCorrecta =
					puedeMover =
					----
					hacerMovimiento = if destinoLibre then moverSimple else moverConSalto
					destinoLibre = ( (f posicionDirecta) == Nothing)
					moverSimple = Maybe (J (cambiaColor col) tableroDeMueve)
					moverConSalto = Maybe (J (cambiaColor col) tableroDeSalto)
					tableroDeMueve = sacar pos (poner posicionDirecta fichaAMover (T f))
					tableroDeSalto = sacar posDirecta (sacar pos (poner posicionDeSalto fichaAMover (T f)) )
					posicionDirecta = damePosicion (M pos dir)
					posicionDeSalto = damePosicion (M posicionDirecta dir)
					fichaAMover = dameFicha (f pos)
					----
						
-}

-- devuelve la posicion del tablero en donde va a quedar la ficha que se quiere mover
-- OJO: Si quiere "comer" una ficha propia o se sale de rango, devuelve Nothing
posLlegada :: Movimiento -> Juego -> Maybe Posicion
posLlegada (M pos dir) (J col (T f)) = if posDirectaLibre then Just posDirecta else posNoDirecta 
		where 
			posDirecta = posDeMoverDirecto (M pos dir)
			posNoDirecta = if posNoDirectaLibre && comidaDisponible then Just posDeMoverDirecto (M posicionDeMovimientoDirecto dir) else Nothing


enTablero :: Posicion -> Bool
enTablero p = elem p posicionesValidas
{-}if (saleDeRango posicionFinal) then Nothing else posicionFinal
						where 
							saleDeRango = \mp -> elem (damePosicion mp) posicionesValidas
							posicionFinal = if posDirectaLibre then posDirecta else posDeComidaSiCome 
							posDirectaLibre = f (posicionDeMovimientoDirecto) == Nothing
							posDirecta = (Just posicionDeMovimientoDirecto )
							posicionDeMovimientoDirecto = posDeMoverDirecto (M pos dir)
							posicionDeComidaSiCome = posDeMoverDirecto (M posicionDeMovimientoDirecto dir)
							posDeComidaSiCome = if hayFichaOponente then (Just posicionDeComidaSiCome) else Nothing
							hayFichaOponente = col /= colorDeFicha (dameFicha (f posicionDeMovimientoDirecto))
-}							
						
--dado un movimiento, devuelve la posicion en donde "desembocaria" ese movimiento (si hay ficha)
posDeMoverDirecto :: Movimiento -> Posicion
posDeMoverDirecto (M pos TL) = ( chr (ord (fst pos) - 1), (snd pos) + 1 )
posDeMoverDirecto (M pos TR) = ( chr (ord (fst pos) + 1), (snd pos) + 1 )
posDeMoverDirecto (M pos BL) = ( chr (ord (fst pos) - 1), (snd pos) - 1 )
posDeMoverDirecto (M pos BR) = ( chr (ord (fst pos) + 1), (snd pos) - 1 )

--cambia el color
cambiaColor :: Color -> Color
cambiaColor Blanca = Negra
cambiaColor Negra = Blanca

--devuelve la ficha de una Maybe Ficha (ES PRECONDICION QUE SEA JUST)
dameFicha :: Maybe Ficha -> Ficha
dameFicha (Just f) = f

--devuelve la posicion de una Maybe Posicion (ES PRECONDICION QUE SEA JUST)
damePosicion :: Maybe Posicion -> Posicion
damePosicion (Just p) = p

--devuelve el color de una ficha
colorDeFicha :: Ficha -> Color
colorDeFicha (Simple c) = c
colorDeFicha (Reina c) = c



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


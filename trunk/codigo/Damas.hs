module Damas where

import Tablero

----------------------- INCLUIDAS POR NOSOTROS---------------------------

import Char
import Maybe

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
  j1 == j2 = tablero j1 == tablero j2 && colorJ j1 == colorJ j2

arbolDeJugadas :: Juego -> ArbolJugadas
arbolDeJugadas j = Nodo ([], j) $ zipWith agmov movs hijos
  where agmov m (Nodo (ms, r) hs) = Nodo ((m:ms), r) (map (agmov m) hs)
        movsJuegos = movimientosPosibles j
        movs = map fst movsJuegos
        hijos = map (arbolDeJugadas . snd) movsJuegos

---- Ejercicios ----

-- Ejercicio 3

mover :: Movimiento -> Juego -> Maybe Juego
--mover mov jue = Nothing

mover m j = if (elMovimientoDirectoEsInvalido || laCapturaEsInvalida) then Nothing else moverSegunSiEsSimpleOCaptura

		where
			elMovimientoDirectoEsInvalido = ( (not origenEnRango) || (not destino1EnRango) || 
							(not hayFichaEnOrigen) || (not mueveElJugadorCorrespondiente) ||
							(not mueveEnDireccionCorrecta) )
			
			laCapturaEsInvalida = (not elMovimientoDirectoEsInvalido) && ((not destino1Vacio) && (seVaAAutoCapturar || (not destino2EnRango) || (not destino2Vacio)) )
			
			moverSegunSiEsSimpleOCaptura = 	if (destino1Vacio)
								then realizarMovimiento origen destino1 j
								else realizarMovimiento origen destino2 j
			origen = posMov m
			destino1 = posDeMoverDirecto m
			destino2 = posDeMoverDirecto (M destino1 (dirMov m))
			fichaOrigen = fromJust (contenido origen (tablero j))
			fichaDestino1 = fromJust (contenido destino1 (tablero j))
			origenEnRango = enRango origen
			destino1EnRango = enRango destino1
			hayFichaEnOrigen = not (estaVacia origen (tablero j))
			mueveElJugadorCorrespondiente = colorF fichaOrigen == colorJ j
			destino1Vacio = estaVacia destino1 (tablero j)
			seVaAAutoCapturar = colorF fichaDestino1 == colorJ j
			destino2EnRango = enRango destino2
			destino2Vacio = estaVacia destino2 (tablero j)
			mueveEnDireccionCorrecta = (esNegraSimple && mueveHaciaAbajo) || (esBlancaSimple && mueveHaciaArriba) || esReina fichaOrigen
			esNegraSimple = (colorF fichaOrigen == Negra) && (not (esReina fichaOrigen))
			esBlancaSimple = (colorF fichaOrigen == Blanca) && (not (esReina fichaOrigen))
			mueveHaciaAbajo = (dirMov m == BL) || (dirMov m == BR)
			mueveHaciaArriba = (dirMov m == TL) || (dirMov m == TR)
							
		
						
--dado un movimiento, devuelve la posicion en donde "desembocaria" ese movimiento
posDeMoverDirecto :: Movimiento -> Posicion
posDeMoverDirecto (M pos TL) = ( chr (ord (fst pos) - 1), (snd pos) + 1 )
posDeMoverDirecto (M pos TR) = ( chr (ord (fst pos) + 1), (snd pos) + 1 )
posDeMoverDirecto (M pos BL) = ( chr (ord (fst pos) - 1), (snd pos) - 1 )
posDeMoverDirecto (M pos BR) = ( chr (ord (fst pos) + 1), (snd pos) - 1 )

--cambia el color
cambiaColor :: Color -> Color
cambiaColor Blanca = Negra
cambiaColor Negra = Blanca


--devuelve el color de una ficha
colorDeFicha :: Ficha -> Color
colorDeFicha (Simple c) = c
colorDeFicha (Reina c) = c

--devuelve un nuevo juego igual al pasado como parametro pero en el que mueve una ficha desde la posicion origen hasta la posicion destino
--PRE: el movimiento a realizar es un movimiento valido
realizarMovimiento :: Posicion -> Posicion -> Juego -> Maybe Juego
realizarMovimiento origen destino j = Just (J nuevoColor tableroNuevo) 
					where
						tableroViejo = tablero j
						fichaVieja = fromJust (contenido origen tableroViejo)
						tableroNuevo = sacar origen ( poner destino fichaNueva tableroViejo)
						nuevoColor = cambiaColor (colorJ j)
						fichaNueva = 	if (llegoAlFondo destino (colorJ j)) 
										then Reina (colorF fichaVieja)
										else fichaVieja

llegoAlFondo :: Posicion -> Color -> Bool
llegoAlFondo p c = ((snd p == 1) && (c == Negra)) || ((snd p == 8) && (c == Blanca))


-- Ejercicio 4
movimientosPosibles :: Juego -> [(Movimiento, Juego)]
movimientosPosibles j = 	[((M pos dir), fromJust (mover (M pos dir) j)) | 
							pos <- posicionesValidas, dir <- [TL, TR, BL, BR], esMovimientoValido (M pos dir) j]
							where
								esMovimientoValido mov = \game -> ((mover mov game) /= Nothing)


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




-------------------- OBSERVADORES PARA TIPO JUEGO --------------------

tablero :: Juego -> Tablero
tablero (J col t) = t

colorJ ::Juego -> Color
colorJ (J col t) = col

-------------------- OBSERVADORES PARA TIPO MOVIMIENTO --------------------

dirMov :: Movimiento -> Direccion
dirMov (M p d) = d

posMov :: Movimiento -> Posicion
posMov (M p d) = p


module Damas where

import Tablero
import Char
import Maybe

data Juego = J Color Tablero
data Movimiento = M Posicion Direccion
  deriving Show

data Arbol a = Nodo a [Arbol a] deriving Show
type ArbolJugadas = Arbol ([Movimiento], Juego)

type Valuacion = Juego -> Double

---- Igualdades ----

instance (Eq a) => Eq (Arbol a) where
	a1 == a2 = ((vNodo a1) == (vNodo a2)) && (hijos a1 == hijos a2)

instance Eq (Movimiento) where
	a1 == a2 = ((posMov a1) == (posMov a2)) && (dirMov a1 == dirMov a2)

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
mover m j = if (elMovimientoDirectoEsInvalido || laCapturaEsInvalida) then Nothing else moverSegunSiEsSimpleOCaptura

		where
			elMovimientoDirectoEsInvalido = ( (not origenEnRango) || (not destino1EnRango) ||
							(not hayFichaEnOrigen) || (not mueveElJugadorCorrespondiente) ||
							(not mueveEnDireccionCorrecta) )

			laCapturaEsInvalida = (not elMovimientoDirectoEsInvalido) && ((not destino1Vacio) && (seVaAAutoCapturar || (not destino2EnRango) || (not destino2Vacio)) )

			moverSegunSiEsSimpleOCaptura =
			          if (destino1Vacio)
								then realizarMovimiento origen destino1 j
								else realizarMovimiento origen destino2 juegoSinLaFichaAComer
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
			juegoSinLaFichaAComer = J (colorJ j) (sacar destino1 (tablero j))


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

--devuelve un nuevo juego igual al pasado como parametro pero en el que mueve una ficha desde la posicion origen hasta la posicion destino,cambiando de turno de jugador
--PRE: el movimiento a realizar es un movimiento valido
realizarMovimiento :: Posicion -> Posicion -> Juego -> Maybe Juego
realizarMovimiento origen destino j = Just (J nuevoColor tableroNuevo)
					where
						tableroViejo = tablero j
						fichaVieja = fromJust (contenido origen tableroViejo)
						tableroNuevo = sacar origen ( poner destino fichaNueva tableroViejo)
						nuevoColor = cambiaColor (colorJ j)
						fichaNueva =
						       	if (llegoAlFondo destino (colorJ j))
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
foldArbol :: (a -> [b] -> b) -> Arbol a -> b
foldArbol f (Nodo x ys) = f x (map (foldArbol f) ys)

-- Ejercicio 6
podar :: Int -> Arbol a -> Arbol a
podar = flip podar'

podar' :: Arbol a -> Int -> Arbol a
podar' = foldArbol (\val rec n -> if (n==0) then Nodo val [] else Nodo val (map (aplicar (n-1)) rec))
		where	aplicar n f = f n


-- Ejercicio 7
mejorMovimiento :: Valuacion -> ArbolJugadas -> Movimiento
mejorMovimiento v aj = head (snd (minimax v aj))

-- Para esta funcion se presento el problema de como alternar los maximos y minimos entre los distintos niveles
-- del arbol. Una de las ideas que surgieron fue la de utilizar siempre el minimo de los valores negados, es decir:
-- en vez de calcular max (x1, x2, x3) se puede calcular -min (-x1, -x2, -x3).
-- El problema con esto fue que no encontramos una manera de lograr un tipo de recursion que comenzara a evaluar
-- desde la raiz del arbol alternadamente y no desde las hojas con el fold que definimos.
-- La segunda propuesta consistio en darnos cuenta si habia que evaluar un maximo o minimo de acuerdo al nivel
-- del arbol en el que se encuentra el nodo. Aqui salieron dos ideas, la primera fue utilizar un contador agregando 
-- un parametro extra a minimax' y para los niveles impares calcular minimo y para los pares maximo. 
-- Finalmente la idea que se implemento fue la de agregar un parametro a la funcion de evaluacion, el cual es el
-- color del jugador que tiene el turno en la raiz, asi se puede comparar con el color del jugador que tiene el
-- turno en cada nodo y evaluar apropiadamente.
minimax :: Valuacion -> ArbolJugadas -> (Double, [Movimiento])
minimax fVal arbol = foldArbol 	(\movs_juego listaRec ->
										  if (null listaRec) then (valuacion (snd movs_juego),fst movs_juego)
											else (minimaValuacion listaRec, movimientos listaRec)
								) arbol--listaRec :: [(Double, [Movimiento])]
							where
								valuacion juego = valuacionConveniente (colorJ (snd (vNodo arbol))) fVal juego
								movimientos l_V_lM = ((dameSeconds l_V_lM)!!indiceDelMinimo l_V_lM)
								indiceDelMinimo l_V_lM = dameIndice (minimaValuacion l_V_lM) (dameFirsts l_V_lM)
								minimaValuacion l_V_lM = minL (dameFirsts l_V_lM)

minL :: Ord a => [a] -> a
minL = foldr1 (\x rec -> if (x<=rec) then x else rec)

--dameFirsts :: [(a,b)] -> [a]
dameFirsts = map (fst)

--dameSeconds :: [(a,b)] -> [b]
dameSeconds = map (snd)

--PRE dameIndice: e debe estar en la lista
--dameIndice :: Eq a=> a -> [a] -> Int
dameIndice e = foldr (\x rec -> if (e == x) then 0 else 1 + rec) 0

--Usando esta valuacion, cuando haga minimax, siempre tengo que tomar el maximo valor de las valuaciones en cada paso
valuacionConveniente :: Color -> Valuacion -> Juego -> Double
valuacionConveniente c v j = if ( (colorJ j) == c) then -(v j) else v j



-- Ejercicio 8
ganador :: Juego -> Maybe Color
ganador j =
          if (null (movimientosPosibles j))
					then Just (cambiaColor (colorJ j))
					else (
					      if (null (movimientosPosibles juegoOponente))
								then Just (colorJ j)
								else Nothing
						 )
					where juegoOponente = J (cambiaColor (colorJ j)) (tablero j)

-- Ejercicio 9
valuacionDamas :: Juego -> Double
valuacionDamas j =
           	if (((ganador j) == Nothing) &&  ((ganador juegoOponente) == Nothing))
						then calculoValuacion
						else 	(beta ganaJugadorActual) * 1 + (beta ganaJugadorOponente) * (-1)
							where
								calculoValuacion = 2*(numeradorDelCalculo / denominadorDelCalculo) - 1
								numeradorDelCalculo =  fromIntegral ( (2*cantReinasDelJugador j) + cantSimplesDelJugador j)
								denominadorDelCalculo = fromIntegral ( (2*cantReinasTotales j) + cantSimplesTotales j)
								juegoOponente = J (cambiaColor (colorJ j)) (tablero j)
								ganaJugadorActual = ( (ganador j) == (Just (colorJ j)) )
								ganaJugadorOponente = ( (ganador juegoOponente) == (Just (cambiaColor (colorJ j))) )


beta b = if b then 1 else 0

cantReinasTotales j = cantFichaDeterminada (esReina) j

cantSimplesTotales j = cantFichaDeterminada (esSimple) j

cantReinasDelJugador j = cantFichaDeterminada (esReinaYDeColor) j
  where esReinaYDeColor ficha = (esReina ficha) && ((colorF ficha) == colorJ j)

cantSimplesDelJugador j = cantFichaDeterminada (esSimpleYDeColor) j
  where esSimpleYDeColor ficha = (esSimple ficha) && ((colorF ficha) == colorJ j)

cantFichaDeterminada :: (Ficha -> Bool) -> Juego -> Int
cantFichaDeterminada f j = foldr (\pos cantReinasParcial ->
                  if ((contenido pos (tablero j) == Nothing) || not (f (fromJust (contenido pos (tablero j)))))
									then cantReinasParcial
									else 1 + cantReinasParcial) 0 (posicionesValidas)

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

-------------------- OBSERVADORES PARA TIPO ARBOL --------------------
vNodo :: Arbol a -> a
vNodo (Nodo x hs) = x

hijos :: Arbol a -> [Arbol a]
hijos (Nodo x hs) = hs


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
mover m j = if (elMovimientoDirectoEsInvalido || laCapturaEsInvalida) then Nothing else moverSegunSiEsSimpleOCaptura

		where
			elMovimientoDirectoEsInvalido = ( (not origenEnRango) || (not destino1EnRango) || 
							(not hayFichaEnOrigen) || (not mueveElJugadorCorrespondiente) ||
							(not mueveEnDireccionCorrecta) )
			
			laCapturaEsInvalida = (not elMovimientoDirectoEsInvalido) && ((not destino1Vacio) && (seVaAAutoCapturar || (not destino2EnRango) || (not destino2Vacio)) )
			
			moverSegunSiEsSimpleOCaptura = 	if (destino1Vacio)
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
foldArbol :: (a -> [b] -> b) -> Arbol a -> b
foldArbol f (Nodo x ys) = f x (map (foldArbol f) ys)



-- Ejercicio 6
--primero las variantes escritas usando recursion
podar :: Int -> Arbol a -> Arbol a
podar = flip podar'

podar' :: Arbol a -> Int -> Arbol a
podar' = foldArbol (\val rec n -> if (n==1) then Nodo val [] else Nodo val (map (aplicar (n-1)) rec))
		where
			aplicar n f = f n
--VER EL CASO BASE. COMO LOS ARBOLES TIENEN AL MENOS UN NIVEL, EL CASO BASE SERÍA PODAR AL PRIMER NIVEL, Y NO AL CEROESIMO
--VER QUE REC ES UNA LISTA DE FUNCIONES QUE VAN DE INT -> ARBOL A			

{------- ESTO ES UN EJEMPLO COMPLETO DE UNA REDUCCION DEL PODAR'--------

DEF: Sea funcion = (\val rec n -> if (n==1) then Nodo val [] else Nodo val (map (aplicar (n-1)) rec))

podar' (Nodo 3 [Nodo 2 [Nodo 1 []], Nodo 5 [Nodo 9 []]]) 2
~~>
foldArbol funcion (Nodo 3 [Nodo 2 [Nodo 1 []], Nodo 5 [Nodo 9 []]]) 2
~~>
if (2==1) then Nodo val [] else Nodo val (map (aplicar (2-1)) rec)   
--rec es el paso recursivo del foldArbol. Es lo que aparece abajo como el segundo map
~~>
Nodo 3 (map (aplicar (1)) (map (foldArbol funcion) [Nodo 2 [Nodo 1 []], Nodo 5 [Nodo 9 []]]))
~~> 
Nodo 3 (map (aplicar (1)) ([foldArbol funcion (Nodo 2 [Nodo 1 []]), foldArbol funcion (Nodo 5 [Nodo 9 []])]))
~~>
Nodo 3 [foldArbol funcion (Nodo 2 [Nodo 1 []]) 1, foldArbol funcion (Nodo 5 [Nodo 9 []]) 1]
~~>
Nodo 3 [funcion 2 (map (foldArbol funcion) [Nodo 1 []]) 1, funcion 5 (map (foldArbol funcion) [Nodo 9 []]) 1 ]
~~>
Nodo 3 [Nodo 2 [], Nodo 5 []]
-}
			


-------------------- EJEMPLOS -----------------------------------
{-
cantNodos = foldArbol (\e rec -> 1 + sum rec)

hojas = foldArbol (\e hijos -> if (null hijos) then e:[] else concat hijos)

distancias = foldArbol (\e hijos -> if (null hijos) then [0] else map (+1) (concat hijos))

altura = foldArbol (\e rec -> if (null rec) then 1 else 1 + (maxLista rec)) where maxLista = foldr1 max
altura2 = (+1).maxLista.distancias where maxLista = foldr1 max
altura3 a = maxLista (map (+1) (distancias a)) where maxLista = foldr1 max
-}
--arbol de pruebas
ap = Nodo 1 [(Nodo 2 [Nodo 4 []]), (Nodo 3 [])]


--------------------- FIN EJEMPLOS ------------------------------

-- Ejercicio 7
mejorMovimiento :: Valuacion -> ArbolJugadas -> Movimiento
mejorMovimiento v aj = head (snd (minimax v aj)) --creo que esta es la idea.... pero no estoy seguro


--esta definicion la dejo pa que hugs no tire errores
minimax :: Valuacion -> ArbolJugadas -> (Double, [Movimiento])
minimax _ _ = (0,[])

{- 
################ ESTE ES EL MINIMAX QUE PENSE (mariano) ####### (hay que testearlo) ############

juegoInicial = J Blanca tableroInicial

minimax :: Valuacion -> ArbolJugadas -> (Double, [Movimiento])
minimax fVal arbol = foldArbol 	(\movs_juego listaRec ->
										if (null listaRec) 
											then (valuacion (snd movs_juego),[])
											else (minimaValuacion listaRec, movimientos movs_juego listaRec)
								) arbol--listaRec :: [(Double, [Movimiento])]
							where
								valuacion juego = valuacionConveniente (colorJ juego) fVal juego
								movimientos movs_juego l_V_lM = (last (fst movs_juego)) : 
																((dameSeconds l_V_lM)!!indiceDelMinimo l_V_lM)
																
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
valuacionConveniente c v j = if ( (colorJ j) == c) then v j else -(v j)

###################################### HASTA ACA ###########################################
-}


-- Ejercicio 8
ganador :: Juego -> Maybe Color
ganador j = if (null (movimientosPosibles j)) then Just (cambiaColor (colorJ j)) else Nothing

-- Ejercicio 9
valuacionDamas :: Juego -> Double
valuacionDamas j = 	if ( (ganador j) == Nothing )
						then calculoValuacion
						else 	(beta ( (ganador j) == (Just (colorJ j)) )) * 1 +
								(beta ( (ganador j) /= (Just (colorJ j)) )) * (-1)
							
							where
								calculoValuacion = 2*(numeradorDelCalculo / denominadorDelCalculo) - 1
								numeradorDelCalculo =  fromIntegral ( (2*cantReinasDelJugador j) + cantSimplesDelJugador j)
								denominadorDelCalculo = fromIntegral ( (2*cantReinasTotales j) + cantSimplesTotales j)


beta b = if b then 1 else 0

cantReinasTotales j = cantFichaDeterminada (esReina) j

cantSimplesTotales j = cantFichaDeterminada (esSimple) j

cantReinasDelJugador j = cantFichaDeterminada (esReinaYDeColor) j
							where esReinaYDeColor ficha = (esReina ficha) && ((colorF ficha) == colorJ j)
							
cantSimplesDelJugador j = cantFichaDeterminada (esSimpleYDeColor) j
							where esSimpleYDeColor ficha = (esSimple ficha) && ((colorF ficha) == colorJ j)
							
cantFichaDeterminada :: (Ficha -> Bool) -> Juego -> Int
cantFichaDeterminada f j = foldr (\pos cantReinasParcial -> 
								if ((contenido pos (tablero j) == Nothing) || 
									not (f (fromJust (contenido pos (tablero j))))) 
									then cantReinasParcial
									else 1 + cantReinasParcial )
							0 (posicionesValidas)

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

-------------------- OBSERVADORES PARA TIPO ARBOL --------------------
vNodo :: Arbol a -> a
vNodo (Nodo x hs) = x

hijos :: Arbol a -> [Arbol a]
hijos (Nodo x hs) = hs

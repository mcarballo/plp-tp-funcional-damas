import Tablero
import Damas
import HUnit
import Maybe

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"triviales" ~: testsTriviales,
	"tablero" ~: testsTablero,
	"damas" ~: testsDamasIniciales,
	"damas2" ~: testsDamasValidos,
	"podarArbol" ~: testPodar,
	"valuacion" ~: testValuacion
	]


testsTriviales = test [
	1 ~=? 1,
	2 ~=? 2,
	[1,2] ~=? [1,2],
	[1,2] ~~? [2,1]
	]

-----------TABLERO-----------------------
testsTablero = test [
  vacio ~=? T (\_ -> Nothing),
  sacar a_1 vacio ~=? T (\_ -> Nothing),
  poner a_1 _n vacio ~=? T (\pos -> if (pos == a_1) then (Just _n) else Nothing),
  poner a_1 _n (poner a_2 _n vacio) ~=? T (\pos -> if (pos == a_1 || pos==a_2) then (Just _n) else Nothing),
  poner a_1 _n (sacar a_2  vacio) ~=? T (\pos -> if (pos == a_1) then (Just _n) else Nothing),
  sacar a_1 (poner a_2 _n vacio) ~=? T (\pos -> if (pos==a_2) then (Just _n) else Nothing),
  sacar a_1 (poner a_1 _n vacio) ~=? T (\_ ->  Nothing),
  tableroInicial2 ~=? tableroInicial
  --explicacion:
  --vacio == vacio
  --sacar al vacio mantiene vacio
  --poner una ficha al vacio se mantiene
  --poner 2 fichas las mantiene
  --sacar al vacio y luego agregar devuelve la agregada
  --poner una ficha en a2 y luego sacar una de a1 deja la de a2.
  --poner y sacar una ficha la quita.
  --tablero completo inicial coincide con el de la catedra.
  ]


tableroInicial2 = poner a_7 _n (poner c_7 _n (poner e_7 _n (poner g_7 _n (poner b_6 _n (poner b_8 _n (poner d_6 _n (poner d_8 _n (poner f_6 _n (poner f_8 _n (poner h_6 _n (poner h_8 _n (poner a_1 _b (poner a_3 _b (poner c_1 _b (poner c_3 _b (poner e_1 _b (poner e_3 _b (poner g_1 _b (poner g_3 _b (poner b_2 _b (poner d_2 _b (poner f_2 _b (poner h_2 _b vacio)))))))))))))))))))))))

-----------DAMAS-----------------------
juegoPrueba = J Blanca tableroInicial

------------------CON TABLERO INCIAL------------
testsDamasIniciales = test (a_testear)

a_testear = map (\movimiento -> mover movimiento juegoPrueba ~=? Nothing) movs_a_nothing

movs_a_nothing =
  inicialmente_invalidos_por_turno ++
  inicialmente_invalidos_por_limites ++
  inicialmente_invalidos_por_origen ++
  invalidos_por_falta_ficha_origen juegoPrueba ++
  invalidos_por_color_del_turno juegoPrueba ++
  invalidos_por_direccion juegoPrueba ++
  inicialmente_invalidos_por_casilla_destino_ocupada

-----------------MOVIMIENTOS VALIDOS-------------
testsDamasValidos = test (a_testear2)

juegoPrueba2 = J Blanca tableroPrueba

a_testear2 = [
  mover (M ('d',4) TL ) juegoPrueba2 ~=? Just (J Negra (poner c_5 _b (sacar d_4 tableroPrueba))),
  mover (M ('d',4) TR ) juegoPrueba2 ~=? Just (J Negra (poner f_6 _b (sacar d_4 (sacar e_5 tableroPrueba)))),
  mover (M h_7 TL) juegoPruebaCoronacionB ~=? Just (J Negra (poner g_8 _B (sacar h_7 tableroPruebaCoronacion))),
  mover (M b_2 BL) juegoPruebaCoronacionN ~=? Just (J Blanca (poner a_1 _N (sacar b_2 tableroPruebaCoronacion))),
  mover (M f_2 BL) juegoPruebaCoronacionB ~=? Just (J Negra (poner e_1 _B (sacar f_2 tableroPruebaCoronacion)))
  ]

tableroPrueba = poner f_4 _b (poner d_6 _n (poner e_5 _n (poner d_4 _b vacio)))

juegoPruebaCoronacionB = J Blanca tableroPruebaCoronacion
juegoPruebaCoronacionN = J Negra tableroPruebaCoronacion
tableroPruebaCoronacion = poner h_7 _B (poner b_2 _n (poner f_2 _B vacio))



------------SECUENCIAS DE MOVIMIENTOS DE PRUEBA----------------
--movimientos invalidos por limites del tablero
inicialmente_invalidos_por_limites = [M ('a',1) BL, M ('a',1) BR, M ('b',8) TR, M ('b',8) TL,M ('h',8) TR, M ('h',2) TL]

--movimientos invalidos por turno (color)
inicialmente_invalidos_por_turno = [M ('b',6) BR,M ('d',6) BL]

--movimientos invalidos por origen invalidos
inicialmente_invalidos_por_origen = [M ('z',9) BL, M ('a',9) BR, M ('s',8) TR, M ('i',8) TL,M (' ',0) TR, M ('h',-2) TL]

--movimientos invalidos por origen sin ficha
invalidos_por_falta_ficha_origen juego = [M pos BL| pos <- posicionesSinFichas juego] ++ [M pos TL| pos <- posicionesSinFichas juego]

--movimientos invalidos por color (turno del oponente)
invalidos_por_color_del_turno juego = [M pos BR| pos <- posicionesConFichasDeColor juego Negra]

--movimientos invalidos por direccion incorrecta (blancas para arriba por ejemplo)
invalidos_por_direccion juego = [M pos TR| pos <- posicionesSimplesDeColor juego Negra]++[M pos TL| pos <- posicionesSimplesDeColor juego Negra]

--movimientos invalidos por destino ocupado
inicialmente_invalidos_por_casilla_destino_ocupada = [M ('a',1) TR, M ('c',1) TL, M ('h',2) TL]



------------FIN SECUENCIAS ----------------
----------- TESTS PODAR -------------------


arbol1 = Nodo 1 [Nodo 2 [Nodo 3 []]]
arbol2 = infinito (Nodo 0 [])


infinito ab = Nodo (vNodo ab) [ infinito (Nodo ((vNodo ab)+1) []) ]


testPodar = [
	podar 1 arbol1 ~=? Nodo 1 [],
	podar 1 arbol2 ~=? Nodo 0 [],
	podar 2 arbol2 ~=? Nodo 0 [Nodo 1 []],
	podar 3 arbol2 ~=? Nodo 0 [Nodo 1 [Nodo 2 []]],
	podar 4 arbol2 ~=? Nodo 0 [Nodo 1 [Nodo 2 [Nodo 3 []]]]
  ]


--TODO: testear el "podar", "mejorMovimiento" (minimax), "ganador" y "valuacionDamas"







-------------- TESTS VALUACIONES --------------

tabGananBlancas = poner f_6 _b (poner g_7 _b (poner h_8 _n vacio))

testValuacion = [
		valuacionDamas (J Blanca tabGananBlancas) ~=? 1
	]



-----------AYUDAS-----------------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

(~~) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Bool
expected ~~ actual = (sort expected) == (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

-- constantes para que los tests sean más legibles
_n = Simple Negra
_N = Reina Negra
_b = Simple Blanca
_B = Reina Blanca

a_1 = ('a',1::Int)
b_1 = ('b',1::Int)
c_1 = ('c',1::Int)
d_1 = ('d',1::Int)
e_1 = ('e',1::Int)
f_1 = ('f',1::Int)
g_1 = ('g',1::Int)
h_1 = ('h',1::Int)

a_2 = ('a',2::Int)
b_2 = ('b',2::Int)
c_2 = ('c',2::Int)
d_2 = ('d',2::Int)
e_2 = ('e',2::Int)
f_2 = ('f',2::Int)
g_2 = ('g',2::Int)
h_2 = ('h',2::Int)

a_3 = ('a',3::Int)
b_3 = ('b',3::Int)
c_3 = ('c',3::Int)
d_3 = ('d',3::Int)
e_3 = ('e',3::Int)
f_3 = ('f',3::Int)
g_3 = ('g',3::Int)
h_3 = ('h',3::Int)

a_4 = ('a',4::Int)
b_4 = ('b',4::Int)
c_4 = ('c',4::Int)
d_4 = ('d',4::Int)
e_4 = ('e',4::Int)
f_4 = ('f',4::Int)
g_4 = ('g',4::Int)
h_4 = ('h',4::Int)

a_5 = ('a',5::Int)
b_5 = ('b',5::Int)
c_5 = ('c',5::Int)
d_5 = ('d',5::Int)
e_5 = ('e',5::Int)
f_5 = ('f',5::Int)
g_5 = ('g',5::Int)
h_5 = ('h',5::Int)

a_6 = ('a',6::Int)
b_6 = ('b',6::Int)
c_6 = ('c',6::Int)
d_6 = ('d',6::Int)
e_6 = ('e',6::Int)
f_6 = ('f',6::Int)
g_6 = ('g',6::Int)
h_6 = ('h',6::Int)

a_7 = ('a',7::Int)
b_7 = ('b',7::Int)
c_7 = ('c',7::Int)
d_7 = ('d',7::Int)
e_7 = ('e',7::Int)
f_7 = ('f',7::Int)
g_7 = ('g',7::Int)
h_7 = ('h',7::Int)

a_8 = ('a',8::Int)
b_8 = ('b',8::Int)
c_8 = ('c',8::Int)
d_8 = ('d',8::Int)
e_8 = ('e',8::Int)
f_8 = ('f',8::Int)
g_8 = ('g',8::Int)
h_8 = ('h',8::Int)


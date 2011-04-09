module Interfaz where

import Char
import Maybe
import IO

import Tablero
import Damas

inicial :: Juego
inicial = J Blanca tableroInicial

-- calcula el mejor movimiento a n niveles
-- de profundidad
proximo :: Int -> Juego -> Movimiento
proximo nivel juego = mejorMovimiento valuacionDamas (podar nivel (arbolDeJugadas juego))

realizarMejorJugada :: Int -> Juego -> Juego
realizarMejorJugada nivel tab = fromJust $ mover (proximo nivel tab) tab

data TipoJugador = Humano | Maquina Int

jugar :: TipoJugador -> TipoJugador -> Juego -> IO ()
jugar jugB jugN juego = jugarConAnt jugB jugN juego juego

jugarConAnt :: TipoJugador -> TipoJugador -> Juego -> Juego -> IO ()
jugarConAnt jugB jugN juegoAnt juego@(J turno tablero) = do
  putStr $ mostrarJ juegoAnt juego
  let gan = ganador juego in
    if isNothing gan
     then do
       juego' <- proxJuego (jugador turno) juego
       jugarConAnt jugB jugN juego juego'
     else do
       putStr "\n~~ Final del juego! ~~\n"
       putStr $ "Ganan las " ++ show (fromJust gan) ++ "s\n"     
  where
    jugador Blanca = jugB
    jugador Negra  = jugN

proxJuego :: TipoJugador -> Juego -> IO Juego
proxJuego Humano juego@(J turno _) = do
  putStr $ "Introducir movimiento (" ++ show turno ++ "s):\n"
  putStr "Movimientos posibles: "
  putStr (movsPosibles juego)
  putStr $ "\n> "
  lupper <- getLine
  let l = normalizar lupper in
    if not (entradaValida l)
      then do
        putStr $ "Entrada invalida.\n"
        proxJuego Humano juego 
      else
        let mov = M (pos l) (dir [l !! 2, l !! 3])
            res = mover mov juego in
          if isNothing res
            then do
              putStr $ "Movimiento invalido.\n"
              proxJuego Humano juego 
            else
              return $ fromJust res
  where
    movsPosibles juego = join ", " $
                         map (showmov . fst) $
                         movimientosPosibles juego
    pos l = (l !! 0, ord (l !! 1) - ord '0')
    dir "tl" = TL
    dir "tr" = TR
    dir "bl" = BL
    dir "br" = BR
    entradaValida l = length l == 4 &&
                      (l !! 0) `elem` ['a'..'h'] &&
                      (l !! 1) `elem` ['1'..'8'] &&
                      (l !! 2) `elem` "bt" && 
                      (l !! 3) `elem` "lr"
proxJuego (Maquina nivel) juego@(J turno _) = do
  putStr $ "Pensando... (" ++ show turno ++ "s)\n"
  let mov = proximo nivel juego
      res = mover mov juego in do
    seq mov $ putStr $ "MOVI: " ++ showmov mov ++ "\n"
    if isNothing res
      then do
        putStr $ "La maquina eligio un movimiento invalido.\n"
        putStr $ "Seguramente es un error.\n"
        putStr $ "Movimiento elegido: " ++ showmov mov
        return $ error "[juego invalido]"
      else
        return $ fromJust res

join _   [] = "<nada>"
join sep xs = foldr1 (\x r -> x ++ sep ++ r) $ xs

showmov (M (col, fil) dir) = normalizar ([col] ++ show fil ++ show dir)
normalizar = filter (/= ' ') . map toLower

mostrarJ (J turno0 tablero0) (J turno tablero) =
  "\n--Juegan las " ++ show turno ++ "s--\n" ++ mostrarT tablero0 tablero

mostrarT (T tablero0) (T tablero) = 
    "   a b c d e f g h  \n" ++
    "  ----------------- \n" ++
    concatMap showFil [8,7..1] ++
    "  ----------------- \n" ++
    "   a b c d e f g h  \n"
  where
    showFil fil = show fil ++ " " ++
                  concatMap (flip showCol fil) ['a'..'h'] ++ lprp 'i' fil ++ " " ++
                  show fil ++ "\n"
    showCol col fil = lprp col fil ++ p (tablero0 (col,fil)) (tablero (col,fil))
    lprp col fil
      | isNothing (tablero0 (col, fil)) &&
        not (isNothing (tablero (col, fil))) = "("
      | col > 'a' &&
        isNothing (tablero0 (colAnt, fil)) &&
        not (isNothing (tablero (colAnt, fil))) = ")"
      | otherwise = "|"
      where
        colAnt = chr (ord col - 1)
    p (Just _) Nothing         = "."
    p _ Nothing                = " "
    p _ (Just (Simple Blanca)) = "b"
    p _ (Just (Reina Blanca))  = "B"
    p _ (Just (Simple Negra))  = "n"
    p _ (Just (Reina Negra))   = "N"


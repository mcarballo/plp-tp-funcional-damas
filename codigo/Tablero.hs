module Tablero where

import Char
import Maybe

data Color = Blanca | Negra deriving (Show, Eq)
data Ficha = Simple Color | Reina Color deriving (Show, Eq)
type Posicion = (Char, Int)
data Tablero = T (Posicion -> Maybe Ficha)
data Direccion = TL | TR | BR | BL deriving (Eq, Ord, Show)

---- Funciones de regalo ----

tableroInicial :: Tablero
tableroInicial = T f
  where
	f (i,j)
	  | j `elem` [1..3] && negro = Just (Simple Blanca)
	  | j `elem` [6..8] && negro = Just (Simple Negra)
	  | otherwise = Nothing
	  where negro = (ord i - ord 'a' + j) `mod` 2 /= 0

instance Show Tablero where
  show (T tablero) = 
      "   a b c d e f g h  \n" ++
      "  ----------------- \n" ++
      concatMap showFil [8,7..1] ++
      "  ----------------- \n" ++
      "   a b c d e f g h  \n"
    where
      showFil fil = show fil ++ " " ++
                    concatMap (showCol fil) ['a'..'h'] ++ "| " ++
                    show fil ++ "\n"
      showCol fil col = "|" ++ p (tablero (col,fil))
      p Nothing = " "
      p (Just (Simple Blanca)) = "b"
      p (Just (Reina Blanca)) = "B"
      p (Just (Simple Negra)) = "n"
      p (Just (Reina Negra)) = "N"

---- Ejercicios ----

-- Ejercicio 1
-- vacio :: Tablero

-- Ejercicio 2
-- contenido :: Posicion -> Tablero -> Maybe Ficha
-- poner :: Posicion -> Ficha -> Tablero -> Tablero
-- sacar :: Posicion -> Tablero -> Tablero


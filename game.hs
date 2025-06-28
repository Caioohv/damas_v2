module Main where

import Data.List (intercalate)

data Piece = Pawn | King deriving (Eq, Show)
data Player = Red | Black deriving (Eq, Show)
data Square = Empty | Occupied Player Piece deriving (Eq, Show)
type Position = (Int, Int)
type Board = [[Square]]
type Move = (Position, Position)

data GameState = GameState
    { board :: Board
    , currentPlayer :: Player
    , gameOver :: Bool
    , winner :: Maybe Player
    } deriving (Show)

initialBoard :: Board
initialBoard = 
    [ [Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn]
    , [Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty]
    , [Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn, Empty, Occupied Black Pawn]
    , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    , [Occupied Red Pawn, Empty, Occupied Red Pawn, Empty, Occupied Red Pawn, Empty, Occupied Red Pawn, Empty]
    , [Empty, Occupied Red Pawn, Empty, Occupied Red Pawn, Empty, Occupied Red Pawn, Empty, Occupied Red Pawn]
    , [Occupied Red Pawn, Empty, Occupied Red Pawn, Empty, Occupied Red Pawn, Empty, Occupied Red Pawn, Empty]
    ]

initialGameState :: Player -> GameState
initialGameState startPlayer = GameState
    { board = initialBoard
    , currentPlayer = startPlayer
    , gameOver = False
    , winner = Nothing
    }

showSquare :: Square -> String
showSquare Empty = " "
showSquare (Occupied Red Pawn) = "r"
showSquare (Occupied Red King) = "R"
showSquare (Occupied Black Pawn) = "b"
showSquare (Occupied Black King) = "B"

showBoard :: Board -> String
showBoard board = 
    "  0 1 2 3 4 5 6 7\n" ++
    intercalate "\n" (zipWith showRow [0..] board) ++
    "\n"
  where
    showRow i row = show i ++ " " ++ intercalate " " (map showSquare row)

main :: IO ()
main = putStrLn "Checkers game - starting..."
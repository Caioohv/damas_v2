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

getSquare :: Board -> Position -> Maybe Square
getSquare board (row, col)
    | row >= 0 && row < 8 && col >= 0 && col < 8 = Just (board !! row !! col)
    | otherwise = Nothing

setSquare :: Board -> Position -> Square -> Board
setSquare board (row, col) square =
    take row board ++ 
    [take col (board !! row) ++ [square] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

isValidPosition :: Position -> Bool
isValidPosition (row, col) = row >= 0 && row < 8 && col >= 0 && col < 8

isPieceOf :: Player -> Square -> Bool
isPieceOf player (Occupied p _) = p == player
isPieceOf _ Empty = False

getPlayerPieces :: Board -> Player -> [Position]
getPlayerPieces board player = 
    [(r, c) | r <- [0..7], c <- [0..7], isPieceOf player (board !! r !! c)]

pawnDirections :: Player -> [Position]
pawnDirections Red = [(-1, -1), (-1, 1)]  -- Red moves up
pawnDirections Black = [(1, -1), (1, 1)]   -- Black moves down

kingDirections :: [Position]
kingDirections = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

opponent :: Player -> Player
opponent Red = Black
opponent Black = Red

isSimpleMove :: Board -> Player -> Move -> Bool
isSimpleMove board player ((fromR, fromC), (toR, toC)) =
    case getSquare board (fromR, fromC) of
        Just (Occupied p piece) | p == player ->
            case getSquare board (toR, toC) of
                Just Empty ->
                    let directions = case piece of
                            Pawn -> pawnDirections player
                            King -> kingDirections
                        deltaR = toR - fromR
                        deltaC = toC - fromC
                    in (deltaR, deltaC) `elem` directions
                _ -> False
        _ -> False

getCaptureMove :: Board -> Player -> Position -> Position -> Maybe (Move, Position)
getCaptureMove board player from@(fromR, fromC) to@(toR, toC) =
    case getSquare board from of
        Just (Occupied p piece) | p == player ->
            let deltaR = toR - fromR
                deltaC = toC - fromC
                directions = case piece of
                    Pawn -> pawnDirections player
                    King -> kingDirections
                midR = fromR + deltaR `div` 2
                midC = fromC + deltaC `div` 2
                midPos = (midR, midC)
            in if abs deltaR == 2 && abs deltaC == 2 && 
                  (signum deltaR, signum deltaC) `elem` map (\(dr, dc) -> (signum (2*dr), signum (2*dc))) directions
               then case getSquare board midPos of
                   Just (Occupied opponent _) | opponent /= player ->
                       case getSquare board to of
                           Just Empty -> Just ((from, to), midPos)
                           _ -> Nothing
                   _ -> Nothing
               else Nothing
        _ -> Nothing

applyMove :: Board -> Move -> Board
applyMove board ((fromR, fromC), (toR, toC)) =
    case getSquare board (fromR, fromC) of
        Just piece ->
            let newBoard = setSquare board (fromR, fromC) Empty
                finalPiece = promotePiece piece (toR, toC)
            in setSquare newBoard (toR, toC) finalPiece
        Nothing -> board

applyCapture :: Board -> Move -> Position -> Board
applyCapture board move capturedPos =
    let boardAfterMove = applyMove board move
    in setSquare boardAfterMove capturedPos Empty

getAllMoves :: Board -> Player -> [Move]
getAllMoves board player =
    let pieces = getPlayerPieces board player
        simpleMoves = [move | pos <- pieces, move <- getSimpleMoves board player pos]
        captureMoves = [move | pos <- pieces, (move, _) <- getCaptureMovesFrom board player pos]
    in if null captureMoves then simpleMoves else captureMoves

getSimpleMoves :: Board -> Player -> Position -> [Move]
getSimpleMoves board player from@(r, c) =
    case getSquare board from of
        Just (Occupied p piece) | p == player ->
            let directions = case piece of
                    Pawn -> pawnDirections player
                    King -> kingDirections
                candidates = [(r + dr, c + dc) | (dr, dc) <- directions]
                validMoves = [to | to <- candidates, 
                             isValidPosition to,
                             getSquare board to == Just Empty]
            in [(from, to) | to <- validMoves]
        _ -> []

getCaptureMovesFrom :: Board -> Player -> Position -> [(Move, Position)]
getCaptureMovesFrom board player from@(r, c) =
    case getSquare board from of
        Just (Occupied p piece) | p == player ->
            let directions = case piece of
                    Pawn -> pawnDirections player
                    King -> kingDirections
                candidates = [(r + 2*dr, c + 2*dc) | (dr, dc) <- directions]
                validCaptures = [result | to <- candidates,
                               isValidPosition to,
                               Just result <- [getCaptureMove board player from to]]
            in validCaptures
        _ -> []

checkGameOver :: GameState -> GameState
checkGameOver gameState
    | gameOver gameState = gameState
    | otherwise =
        let redPieces = getPlayerPieces (board gameState) Red
            blackPieces = getPlayerPieces (board gameState) Black
            currentMoves = getAllMoves (board gameState) (currentPlayer gameState)
        in if null redPieces
           then gameState { gameOver = True, winner = Just Black }
           else if null blackPieces
           then gameState { gameOver = True, winner = Just Red }
           else if null currentMoves
           then gameState { gameOver = True, winner = Just (opponent (currentPlayer gameState)) }
           else gameState

isValidMove :: Board -> Player -> Move -> Bool
isValidMove board player move =
    move `elem` getAllMoves board player

executeMove :: Board -> Player -> Move -> Board
executeMove board player move =
    case getCaptureMove board player (fst move) (snd move) of
        Just (_, capturedPos) -> applyCapture board move capturedPos
        Nothing -> applyMove board move

promotePiece :: Square -> Position -> Square
promotePiece (Occupied Red Pawn) (0, _) = Occupied Red King
promotePiece (Occupied Black Pawn) (7, _) = Occupied Black King
promotePiece piece _ = piece

main :: IO ()
main = putStrLn "Checkers game - starting..."
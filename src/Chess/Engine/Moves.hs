module Chess.Engine.Moves
    ( Move(..)
    , MoveRule
    , concatMoveRules
    , availableMoves
    , nthMove
    , updateWith
    , pieceRule
    )
where

import           Chess.Engine.State             ( Board
                                                , BoardIx
                                                , Game(..)
                                                , Piece(..)
                                                , PieceType(..)
                                                , Color(..)
                                                , boardRange
                                                )
import           Data.Array.IArray              ( (!)
                                                , assocs
                                                , inRange
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , isNothing
                                                , fromMaybe
                                                )

-- TODO: Checks; hard pin and suicide rules.

-- | A 'Move' record contains the start, end, and possible capture location of a move, including
-- any side effect moves for castling and an updating function to apply to the moved piece.
data Move = Move { movesFrom :: BoardIx
                 , movesTo :: BoardIx
                 , updater :: Piece -> Piece
                 , captures :: Maybe BoardIx
                 , sideEffect :: Maybe Move }

-- | A 'MoveRule' generates a list of possible moves from a given position on the board.
type MoveRule = Board -> BoardIx -> [Move]

-- | Concatenate a list of move rules into one move rule that allows all of them.
concatMoveRules :: [MoveRule] -> MoveRule
concatMoveRules rules board pos = concatMap (\rule -> rule board pos) rules

-- | List the available moves for the current player in a game.
availableMoves :: Game -> [Move]
availableMoves game = concat . mapMaybe movesAt . assocs $ brd
  where
    brd = board game
    movesAt (pos, square) = pieceMoves pos <$> square
    pieceMoves pos piece = pieceRule piece brd pos

-- | Only allow the nth move given by an initial move rule.
nthMove :: Int -> MoveRule -> MoveRule
nthMove n rule = fmap (take 1 . drop (n - 1)) . rule

-- | Only allow moves if a piece predicate is satisfied.
onlyWhen :: (Piece -> Bool) -> MoveRule -> MoveRule
onlyWhen pred rule board pos =
    if maybe False pred (board ! pos) then rule board pos else []

-- | Append a piece updater to each move.
updateWith :: (Piece -> Piece) -> MoveRule -> MoveRule
updateWith pieceUpdater rule = fmap (map moveUpdater) . rule
    where moveUpdater move = move { updater = pieceUpdater . updater move }

moveFrom :: BoardIx -> BoardIx -> Move
moveFrom start end = Move { movesFrom  = start
                          , movesTo    = end
                          , updater    = \piece -> piece { hasMoved = True }
                          , captures   = Nothing
                          , sideEffect = Nothing
                          }

withCaptureAt :: BoardIx -> Move -> Move
withCaptureAt pos move = move { captures = Just pos }

captureFrom :: BoardIx -> BoardIx -> Move
captureFrom start end = withCaptureAt end $ moveFrom start end

validSquare :: BoardIx -> Bool
validSquare = inRange boardRange

emptySquareOn :: Board -> BoardIx -> Bool
emptySquareOn board pos = isNothing $ board ! pos

lineOfSightMove :: (Int, Int) -> MoveRule
lineOfSightMove (dx, dy) board (sx, sy) =
    map (moveFrom (sx, sy))
        . takeWhile (emptySquareOn board)
        . takeWhile validSquare
        $ [ (sx + n * dx, sy + n * dy) | n <- [1 ..] ]

lineOfSightCapture :: (Int, Int) -> MoveRule
lineOfSightCapture (dx, dy) board (sx, sy) =
    map (captureFrom (sx, sy))
        . take 1
        . dropWhile (emptySquareOn board)
        . takeWhile validSquare
        $ [ (sx + n * dx, sy + n * dy) | n <- [1 ..] ]

jumpMove :: (Int, Int) -> MoveRule
jumpMove = nthMove 1 . lineOfSightMove

jumpCapture :: (Int, Int) -> MoveRule
jumpCapture = nthMove 1 . lineOfSightMove

-- | Get the move rule that applies to a given piece.
pieceRule :: Piece -> MoveRule
pieceRule piece = case pieceType piece of
    Pawn -> concatMoveRules
        [ pawnStep color
        , pawnLeap color
        , pawnCapture color left
        , pawnCapture color right
        , enPassant color left
        , enPassant color right
        ]
    Rook ->
        concatMoveRules
            $  map lineOfSightMove    gridLines
            ++ map lineOfSightCapture gridLines
    Knight ->
        concatMoveRules
            $  map jumpMove    knightOffsets
            ++ map jumpCapture knightOffsets
    Bishop ->
        concatMoveRules
            $  map lineOfSightMove    diagonalLines
            ++ map lineOfSightCapture diagonalLines
    Queen ->
        concatMoveRules
            $  map lineOfSightMove    queenLines
            ++ map lineOfSightCapture queenLines
    King ->
        concatMoveRules
            $  map jumpMove    queenLines
            ++ map jumpCapture queenLines
            ++ [castleQueenSide, castleKingSide]
  where
    xor           = (/=)
    (left, right) = (-1, 1)
    color         = pieceColor piece
    gridLines =
        [ (dirx, diry)
        | dirx <- [-1 .. 1]
        , diry <- [-1 .. 1]
        , (dirx /= 0) `xor` (diry /= 0)
        ]
    diagonalLines =
        [ (dirx, diry)
        | dirx <- [-1 .. 1]
        , diry <- [-1 .. 1]
        , (dirx /= 0) && (diry /= 0)
        ]
    knightOffsets = [ (dirx, diry) | dirx <- [-1, 1], diry <- [-2, 2] ]
    queenLines    = gridLines ++ diagonalLines

pawnDirection :: Color -> (Int, Int)
pawnDirection White = (0, 1)
pawnDirection Black = (0, -1)

pawnStep :: Color -> MoveRule
pawnStep = nthMove 1 . lineOfSightMove . pawnDirection

pawnLeap :: Color -> MoveRule
pawnLeap =
    onlyWhen (not . hasMoved)
        . updateWith setPassantTarget
        . nthMove 2
        . lineOfSightMove
        . pawnDirection
    where setPassantTarget piece = piece { enPassantTarget = True }

pawnCapture :: Color -> Int -> MoveRule
pawnCapture color dx = jumpCapture (directionx + dx, directiony)
    where (directionx, directiony) = pawnDirection color

enPassant :: Color -> Int -> MoveRule
enPassant color dx board (sx, sy) = [ passingMove | canPass ]
  where
    (directionx, directiony) = pawnDirection color
    target                   = (sx + dx, sy)
    end                      = (sx + dx + directionx, sy + directiony)
    canPass                  = validSquare end && validSquare target && maybe
        False
        enPassantTarget
        (board ! target)
    passingMove = withCaptureAt target $ moveFrom (sx, sy) end

castleKingSide :: MoveRule
castleKingSide = undefined

castleQueenSide :: MoveRule
castleQueenSide = undefined

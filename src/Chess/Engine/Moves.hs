module Chess.Engine.Moves
    ( Move(..)
    , MoveRule
    , applyMove
    , moveRuleFor
    )
where

import           Chess.Engine.State             ( Board
                                                , BoardIx
                                                , Piece(..)
                                                , PieceType(..)
                                                , boardRange
                                                )
import           Data.Array.IArray              ( (//)
                                                , (!)
                                                , inRange
                                                )
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )

-- | A representation of a move solely as the location before and after the move.
data Move = Move { movesFrom :: BoardIx, movesTo :: BoardIx, postMove :: Board -> Board }

-- | A 'MoveRule' gives a list of possible moves from a given board position.
type MoveRule = Board -> BoardIx -> [Move]

-- | Apply a move to a board. This clears the square the move starts at and copies its state to the
-- square the move ends at, replacing whatever was there.
applyMove :: Move -> Board -> Board
applyMove move board = post $ board // [(from, Nothing), (to, piece)]
  where
    post  = postMove move
    from  = movesFrom move
    to    = movesTo move
    piece = board ! from

-- | Check if a position is valid to move to without taking anything.
validMoveTarget :: Board -> BoardIx -> Bool
validMoveTarget board ix = inRange boardRange ix && isNothing (board ! ix)

-- | A simple move rule has no post move action.
simpleMoveRule :: (Board -> BoardIx -> [BoardIx]) -> MoveRule
simpleMoveRule rule board start =
    [ Move { movesFrom = start, movesTo = target, postMove = id }
    | target <- rule board start
    ]

-- | A line move rule moves by repeatedly shifting by a given offset until the path is blocked.
lineMoveRule :: (Int, Int) -> MoveRule
lineMoveRule (dx, dy) = simpleMoveRule $ \board (startx, starty) -> takeWhile
    (validMoveTarget board)
    [ (startx + n * dx, starty + n * dy) | n <- [1 ..] ]

-- | A push move rule is a line move rule but with the number of shifts by the offset fixed.
pushMoveRule :: Int -> (Int, Int) -> MoveRule
pushMoveRule range offset board =
    take 1 . drop (range - 1) . lineMoveRule offset board

-- | A jump move rule moves by a given offset, possibly skipping over pieces.
jumpMoveRule :: (Int, Int) -> MoveRule
jumpMoveRule (dx, dy) = simpleMoveRule $ \board (startx, starty) ->
    filter (validMoveTarget board) [(startx + dx, starty + dy)]

-- | Combine a list of move rules into a single rule that allows all of them.
concatMoveRules :: [MoveRule] -> MoveRule
concatMoveRules rules board start = concatMap (($ start) . ($ board)) rules

-- | Create a move rule that only applies if a predicate is satisfied.
conditionalMoveRule :: (Board -> BoardIx -> Bool) -> MoveRule -> MoveRule
conditionalMoveRule pred rule board start =
    if pred board start then rule board start else []

-- | Predicate checking if a pawn can leap.
canLeap :: Board -> BoardIx -> Bool
canLeap board position = maybe False (not . hasMoved) (board ! position)

-- | Utility function for creating a capture post-move.
takeAt :: BoardIx -> Board -> Board
takeAt pos = (// [(pos, Nothing)])

-- | The predicate for being able to take en passant for a given x-offset.
canPassant :: Int -> Board -> BoardIx -> Bool
canPassant offset board (pawnx, pawny) =
    inRange boardRange targetPos
        && (case board ! targetPos of
               Just p  -> enPassantTarget p && pieceColor p /= movingColor
               Nothing -> False
           )
  where
    targetPos   = (pawnx + offset, pawny)
    movingColor = pieceColor . fromJust $ board ! (pawnx, pawny)

-- | Create a move rule to capture en passant for a given x-offset.
passantMove :: Int -> MoveRule
passantMove offset board (pawnx, pawny) =
    [ Move { movesFrom = (pawnx, pawny)
           , movesTo   = (pawnx + offset, pawny + 1)
           , postMove  = takeAt (pawnx + offset, pawny)
           }
    ]

-- | Move rule for en passant.
enPassantRule :: MoveRule
enPassantRule = concatMoveRules [enPassant (-1), enPassant 1]
    where enPassant = conditionalMoveRule <$> canPassant <*> passantMove

-- TODO: Update hasMoved and enPassantTarget when moving

-- | Gets the move rule for a given piece type on white's side.
moveRuleFor :: PieceType -> MoveRule
moveRuleFor Pawn = concatMoveRules
    [ pushMoveRule 1 pawnDir
    , conditionalMoveRule canLeap $ pushMoveRule 2 pawnDir
    , enPassantRule
    ]
    where pawnDir = (0, 1)
moveRuleFor Rook = concatMoveRules
    [ lineMoveRule (dx, dy)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , (dx /= 0) `xor` (dy /= 0)
    ]
    where xor = (/=)
moveRuleFor Knight =
    concatMoveRules [ jumpMoveRule (dx, dy) | dx <- [-1, 1], dy <- [-2, 2] ]
moveRuleFor Bishop = concatMoveRules
    [ lineMoveRule (dx, dy)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , (dx /= 0) && (dy /= 0)
    ]
moveRuleFor Queen = concatMoveRules [moveRuleFor Rook, moveRuleFor Bishop]
moveRuleFor King =
    concatMoveRules
        $ castleRule
        : [ pushMoveRule 1 (dx, dy)
          | dx <- [-1 .. 1]
          , dy <- [-1 .. 1]
          , (dx /= 0) || (dy /= 0)
          ]
    where castleRule = undefined

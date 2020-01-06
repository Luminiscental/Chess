module Chess.Engine.Moves
    ( Move(..)
    , MoveRule
    , applyMove
    , applyMoveRule
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
import           Data.Maybe                     ( isNothing )

-- TODO: This can't handle castling / en passant stuff, create a more general type then specialize

-- | A representation of a move solely as the location before and after the move.
data Move = Move { movesFrom :: BoardIx, movesTo :: BoardIx } deriving (Show, Eq)

-- | A 'MoveRule' gives a list of possible targets to move to from a given board position.
type MoveRule = Board -> BoardIx -> [BoardIx]

-- | Apply a move to a board. This clears the square the move starts at and copies its state to the
-- square the move ends at, replacing whatever was there.
applyMove :: Move -> Board -> Board
applyMove move board = board // [(from, Nothing), (to, piece)]
  where
    from  = movesFrom move
    to    = movesTo move
    piece = board ! from

-- | Use a move rule to get a list of available moves.
applyMoveRule :: MoveRule -> Board -> BoardIx -> [Move]
applyMoveRule rule board start = map (Move start) (rule board start)

-- | Check if a position is valid to move to without taking anything.
validMoveTarget :: Board -> BoardIx -> Bool
validMoveTarget board ix = inRange boardRange ix && isNothing (board ! ix)

-- | A line move rule moves by repeatedly shifting by a given offset until the path is blocked.
lineMoveRule :: (Int, Int) -> MoveRule
lineMoveRule (dx, dy) board (startx, starty) = takeWhile
    (validMoveTarget board)
    [ (startx + n * dx, starty + n * dy) | n <- [1 ..] ]

-- | A push move rule is a line move rule but with the number of shifts by the offset fixed.
pushMoveRule :: Int -> (Int, Int) -> MoveRule
pushMoveRule range offset board =
    take 1 . drop (range - 1) . lineMoveRule offset board

-- | A jump move rule moves by a given offset, possibly skipping over pieces.
jumpMoveRule :: (Int, Int) -> MoveRule
jumpMoveRule (dx, dy) board (startx, starty) =
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

-- | Gets the move rule for a given piece type on white's side.
moveRuleFor :: PieceType -> MoveRule
moveRuleFor Pawn = concatMoveRules
    [ pushMoveRule 1 pawnDir
    , conditionalMoveRule canLeap $ pushMoveRule 2 pawnDir
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
moveRuleFor King  = concatMoveRules
    [ pushMoveRule 1 (dx, dy)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , (dx /= 0) || (dy /= 0)
    ]

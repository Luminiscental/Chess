module Chess.Engine.Moves
    ( Move(..)
    , MoveRule
    , applyMove
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

-- TODO: Castling
-- TODO: Reset enPassantTarget in some update loop.
-- TODO: General utilities to list available moves in a position.
-- TODO: Move rules for all pieces, including capture moves.
-- TODO: How can this flip nicely for playing as black while including side-effects/predicates?

-- | A representation of a move solely as the location before and after the move.
data Move = Move { movesFrom :: BoardIx, movesTo :: BoardIx, postMove :: Board -> Board }

-- | A 'MoveRule' gives a list of possible moves from a given board position.
type MoveRule = Board -> BoardIx -> [Move]

-- | Apply a move to a board. Any piece at the target location is discarded.
applyMove :: Move -> Board -> Board
applyMove move board = post $ board // [(from, Nothing), (to, piece)]
  where
    post  = postMove move
    from  = movesFrom move
    to    = movesTo move
    piece = updatePiece <$> board ! from
    updatePiece p = p { hasMoved = True }

-- | Create a simple move rule given a piece updater and a function to generate target locations.
simpleMoveRule
    :: (Piece -> Piece) -> (Board -> BoardIx -> [BoardIx]) -> MoveRule
simpleMoveRule pieceUpdater targetLister board start =
    [ Move { movesFrom = start, movesTo = target, postMove = updateAt target }
    | target <- targetLister board start
    ]
    where updateAt pos board = board // [(pos, pieceUpdater <$> board ! pos)]

-- | Default piece updater, which simply sets 'hasMoved' to 'True'.
defaultPieceUpdater :: Piece -> Piece
defaultPieceUpdater piece = piece { hasMoved = True }

-- | Utility function for creating a capture post-move.
takeAt :: BoardIx -> Board -> Board
takeAt pos = (// [(pos, Nothing)])

-- | Add a capture to a move rule.
withCapture :: BoardIx -> MoveRule -> MoveRule
withCapture target rule board start =
    [ move { postMove = takeAt target . postMove move }
    | move <- rule board start
    ]

-- | Combine a list of move rules into a single rule that allows all of them.
concatMoveRules :: [MoveRule] -> MoveRule
concatMoveRules rules board start = concatMap (($ start) . ($ board)) rules

-- | Create a move rule that only applies if a predicate is satisfied.
conditionalMoveRule :: (Board -> BoardIx -> Bool) -> MoveRule -> MoveRule
conditionalMoveRule pred rule board start =
    if pred board start then rule board start else []

-- | Check if a position is valid to move to without taking anything.
validMoveTarget :: Board -> BoardIx -> Bool
validMoveTarget board ix = inRange boardRange ix && isNothing (board ! ix)

-- | Get target locations along a line of sight given an offset to shift by.
lineTargets :: (Int, Int) -> (Board -> BoardIx -> [BoardIx])
lineTargets (dx, dy) board (startx, starty) = takeWhile
    (validMoveTarget board)
    [ (startx + n * dx, starty + n * dy) | n <- [1 ..] ]

-- | Get target locations of a fixed range along a line of sight given an offset to shift by. 
pushTargets :: Int -> (Int, Int) -> (Board -> BoardIx -> [BoardIx])
pushTargets range offset board =
    take 1 . drop (range - 1) . lineTargets offset board

-- | Get target locations to jump to given a jump offset.
jumpTargets :: (Int, Int) -> (Board -> BoardIx -> [BoardIx])
jumpTargets (dx, dy) board (startx, starty) =
    filter (validMoveTarget board) [(startx + dx, starty + dy)]

-- | The predicate for a pawn being able to jump by 2 squares.
canLeap :: Board -> BoardIx -> Bool
canLeap board pos = maybe False (not . hasMoved) (board ! pos)

-- | The predicate for a pawn being able to take en passant for a given x-offset.
canPassant :: Int -> Board -> BoardIx -> Bool
canPassant xdir board (pawnx, pawny) = inRange boardRange targetPos
    && maybe False enPassantTarget (board ! targetPos)
    where targetPos = (pawnx + xdir, pawny)

-- | Create a move rule to capture en passant for a given x-offset.
passantMoveRule :: Int -> MoveRule
passantMoveRule xdir board (pawnx, pawny) = return $ Move
    { movesFrom = (pawnx, pawny)
    , movesTo   = (pawnx + xdir, pawny + 1)
    , postMove  = takeAt (pawnx + xdir, pawny)
    }

-- | The move rule for a pawn.
pawnRule :: MoveRule
pawnRule = concatMoveRules
    [normalPawnMove, pawnLeap, enPassant left, enPassant right]
  where
    normalPawnMove = simpleMoveRule defaultPieceUpdater normalTargets
    pawnLeap =
        conditionalMoveRule canLeap (simpleMoveRule leapUpdater leapTargets)
    enPassant xdir =
        conditionalMoveRule (canPassant xdir) (passantMoveRule xdir)
    leapUpdater piece = piece { hasMoved = True, enPassantTarget = True }
    normalTargets = pushTargets 1 (0, 1)
    leapTargets   = pushTargets 2 (0, 1)
    left          = -1
    right         = 1

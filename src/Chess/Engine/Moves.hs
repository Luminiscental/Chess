module Chess.Engine.Moves
    ( Move(..)
    , MoveRule
    , applyMove
    , squareThreatenedBy
    , existsCheckAgainst
    , availableMoves
    , pieceRule
    )
where

import           Chess.Engine.State             ( Board
                                                , BoardIx
                                                , Piece(..)
                                                , PieceType(..)
                                                , Color(..)
                                                , boardRange
                                                , nextTurn
                                                )
import           Data.Array.IArray              ( (!)
                                                , (//)
                                                , assocs
                                                , inRange
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , isNothing
                                                , fromMaybe
                                                , fromJust
                                                , listToMaybe
                                                )
import           Control.Monad                  ( mfilter )

-- TODO: Pawn promotion.

-- | A 'Move' record contains the start, end, and possible capture location of a move, including
-- any side effect moves for castling and an updating function to apply to the moved piece. The
-- 'sideEffect' field is used to store additional moves that occur simultaneously, e.g. when castling.
data Move = Move { movesFrom :: BoardIx
                 , movesTo :: BoardIx
                 , updater :: Piece -> Piece
                 , captures :: Maybe BoardIx
                 , sideEffect :: Maybe Move }

-- | A 'MoveRule' generates a list of possible moves from a given position on the board.
type MoveRule = Board -> BoardIx -> [Move]

-- | Apply a 'Move' to a 'Board'.
applyMove :: Move -> Board -> Board
applyMove move board = postMove $ board // changes
  where
    piece    = board ! movesFrom move
    postMove = maybe id applyMove $ sideEffect move
    changes =
        [(movesFrom move, Nothing), (movesTo move, updater move <$> piece)]

-- | Check whether a square is threatened by a piece of a given color.
squareThreatenedBy
    :: Color -- ^ The player to check for threats from
    -> Board -- ^ The board state
    -> BoardIx -- ^ The square to check for threats to
    -> Bool
squareThreatenedBy color board pos = any threatens $ availableMoves color board
    where threatens move = captures move == Just pos

-- | Check if there is a check on board against a given color.
existsCheckAgainst :: Color -> Board -> Bool
existsCheckAgainst color board =
    let enemy = nextTurn color
        pieceIsKing piece =
                pieceType piece == King && pieceColor piece == color
        squareIsKing  = maybe False pieceIsKing
        kingPositions = map fst . filter (squareIsKing . snd) $ assocs board
    in  fromMaybe False $ do
            kingPosition <- listToMaybe kingPositions
            return $ squareThreatenedBy enemy board kingPosition

-- | List the available moves for a given color on a board.
availableMoves :: Color -> Board -> [Move]
availableMoves color board =
    filter noCheck . concat . mapMaybe movesAt . assocs $ board
  where
    movesAt (pos, square) =
        pieceMoves pos <$> mfilter ((== color) . pieceColor) square
    pieceMoves pos piece = pieceRule piece board pos
    noCheck move = not . existsCheckAgainst color $ applyMove move board

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

concatMoveRules :: [MoveRule] -> MoveRule
concatMoveRules rules board pos = concatMap (\rule -> rule board pos) rules

nthMove :: Int -> MoveRule -> MoveRule
nthMove n rule = fmap (take 1 . drop (n - 1)) . rule

onlyWhen :: (Piece -> Bool) -> MoveRule -> MoveRule
onlyWhen pred rule board pos =
    if maybe False pred (board ! pos) then rule board pos else []

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

-- TODO: Implement castling move rules.

castleKingSide :: MoveRule
castleKingSide = undefined

castleQueenSide :: MoveRule
castleQueenSide = undefined

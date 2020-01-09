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
                                                , Game(..)
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

-- | A 'MoveRule' generates a list of possible moves from a given position on the brd.
type MoveRule = Board -> BoardIx -> [Move]

-- | Apply a 'Move' to a 'Board'.
applyMove :: Move -> Board -> Board
applyMove move brd = postMove $ brd // changes
  where
    piece    = brd ! movesFrom move
    postMove = maybe id applyMove $ sideEffect move
    changes =
        [(movesFrom move, Nothing), (movesTo move, updater move <$> piece)]

-- | Check whether a square is threatened by a piece of a given color, returning @True@ even if the
-- only threatening pieces are pinned.
squareThreatenedBy
    :: Color -- ^ The player to check for threats from
    -> Board -- ^ The brd state
    -> BoardIx -- ^ The square to check for threats to
    -> Bool
squareThreatenedBy color brd pos = any threatens $ movesFromColor color brd
    where threatens move = captures move == Just pos

-- | Check if there is a check on brd against a given color.
existsCheckAgainst :: Color -> Board -> Bool
existsCheckAgainst color brd =
    let enemy = nextTurn color
        pieceIsKing piece =
                pieceType piece == King && pieceColor piece == color
        squareIsKing  = maybe False pieceIsKing
        kingPositions = map fst . filter (squareIsKing . snd) $ assocs brd
    in  any (squareThreatenedBy enemy brd) kingPositions

-- | List the available legal moves to the current player.
availableMoves :: Game -> [Move]
availableMoves game = filter noCheck $ movesFromColor color brd
  where
    color = toMove game
    brd   = board game
    noCheck move = not . existsCheckAgainst color $ applyMove move brd

-- | List the available moves to a given player on the board, without disallowing moves that leave
-- the player in check.
movesFromColor :: Color -> Board -> [Move]
movesFromColor color brd = concat . mapMaybe movesAt . assocs $ brd
  where
    movesAt (pos, square) =
        pieceMoves pos <$> mfilter ((== color) . pieceColor) square
    pieceMoves pos piece = pieceRule piece brd pos

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
concatMoveRules rules brd pos = concatMap (\rule -> rule brd pos) rules

nthMove :: Int -> MoveRule -> MoveRule
nthMove n rule = fmap (take 1 . drop (n - 1)) . rule

onlyWhen :: (Piece -> Bool) -> MoveRule -> MoveRule
onlyWhen pred rule brd pos =
    if maybe False pred (brd ! pos) then rule brd pos else []

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
emptySquareOn brd pos = isNothing $ brd ! pos

lineOfSightMove :: (Int, Int) -> MoveRule
lineOfSightMove (dx, dy) brd (sx, sy) =
    map (moveFrom (sx, sy))
        . takeWhile (emptySquareOn brd)
        . takeWhile validSquare
        $ [ (sx + n * dx, sy + n * dy) | n <- [1 ..] ]

lineOfSightCapture :: (Int, Int) -> MoveRule
lineOfSightCapture (dx, dy) brd (sx, sy) =
    map (captureFrom (sx, sy))
        . take 1
        . dropWhile (emptySquareOn brd)
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
enPassant color dx brd (sx, sy) = [ passingMove | canPass ]
  where
    (directionx, directiony) = pawnDirection color
    target                   = (sx + dx, sy)
    end                      = (sx + dx + directionx, sy + directiony)
    canPass                  = validSquare end && validSquare target && maybe
        False
        enPassantTarget
        (brd ! target)
    passingMove = withCaptureAt target $ moveFrom (sx, sy) end

-- TODO: Implement castling move rules.

castleKingSide :: MoveRule
castleKingSide = error "Castling unimplemented"

castleQueenSide :: MoveRule
castleQueenSide = error "Castling unimplemented"

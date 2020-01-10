module Chess.Engine.Moves
    ( Move(..)
    , Action(..)
    , MoveRule
    , ActionRule
    , noCaptures
    , applyMove
    , applyAction
    , squareThreatenedBy
    , existsCheckAgainst
    , checkToAddress
    , availableActions
    , actionsForColor
    , actionsForColorUnchecked
    , pieceRule
    )
where

import           Chess.Util                     ( rangeExclusive
                                                , rangeInclusive
                                                )
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
                                                , isJust
                                                )
import           Control.Monad                  ( mfilter
                                                , guard
                                                )

-- TODO: Pawn promotion.

-- | A 'Move' record contains the start and end locations of a move, including any side effect
-- moves for castling, and an updating function to apply to the moved piece.
data Move = Move { movesFrom :: BoardIx
                 , movesTo :: BoardIx
                 , updater :: Piece -> Piece
                 , sideEffect :: Maybe Move }

-- | An 'Action' represents a 'Move' with metadata about where and whether a capture occurs.
data Action = NoCapture Move | Capture BoardIx Move

-- | A 'MoveRule' generates a list of possible moves from a given position on the board.
type MoveRule = Board -> BoardIx -> [Move]

-- | An 'ActionRule' generates a list of possible actions from a given position on the board.
type ActionRule = Board -> BoardIx -> [Action]

-- | Convert a 'MoveRule' to an 'ActionRule' by assuming no move is a 'Capture'.
noCaptures :: MoveRule -> ActionRule
noCaptures moveRule board start = map NoCapture $ moveRule board start

-- | Apply a 'Move' to a 'Board'.
applyMove :: Move -> Board -> Board
applyMove move brd = postMove $ brd // changes
  where
    piece    = brd ! movesFrom move
    postMove = maybe id applyMove $ sideEffect move
    changes =
        [(movesFrom move, Nothing), (movesTo move, updater move <$> piece)]

-- | Apply an 'Action' to a 'Board'.
applyAction :: Action -> Board -> Board
applyAction (NoCapture move ) board = applyMove move board
applyAction (Capture at move) board = applyMove move $ board // [(at, Nothing)]

-- | Check whether a square is threatened by a piece of a given color, returning @True@ even if the
-- only threatening pieces are pinned.
squareThreatenedBy
    :: Color -- ^ The player to check for threats from
    -> Board -- ^ The board state
    -> BoardIx -- ^ The square to check for threats to
    -> Bool
squareThreatenedBy color brd pos = any threatens
    $ actionsForColorUnchecked color boardWithTarget
  where
    boardWithTarget = brd // [(pos, Just $ Piece Pawn otherColor False False)]
    otherColor      = nextTurn color
    threatens action = case action of
        Capture at _ -> at == pos
        _            -> False

-- | Check if there is a check on board against a given color.
existsCheckAgainst :: Color -> Board -> Bool
existsCheckAgainst color brd =
    let enemy = nextTurn color
        pieceIsKing piece =
                pieceType piece == King && pieceColor piece == color
        squareIsKing  = maybe False pieceIsKing
        kingPositions = map fst . filter (squareIsKing . snd) $ assocs brd
    in  any (squareThreatenedBy enemy brd) kingPositions

-- | Check if there is a check that the current player needs to address.
checkToAddress :: Game -> Bool
checkToAddress game = existsCheckAgainst color brd
  where
    color = toMove game
    brd   = board game

-- | List the available legal moves to the current player.
availableActions :: Game -> [Action]
availableActions = actionsForColor <$> toMove <*> board

-- | List the available legal moves to a given player on the board.
actionsForColor :: Color -> Board -> [Action]
actionsForColor color brd = filter noCheck $ actionsForColorUnchecked color brd
  where
    noCheck action = not . existsCheckAgainst color $ applyAction action brd

-- | List the available moves to a given player on the board, without disallowing moves that leave
-- the player in check.
actionsForColorUnchecked :: Color -> Board -> [Action]
actionsForColorUnchecked color brd = concat . mapMaybe movesAt . assocs $ brd
  where
    movesAt (pos, square) =
        pieceMoves pos <$> mfilter ((== color) . pieceColor) square
    pieceMoves pos piece = pieceRule piece brd pos

-- | Get the 'ActionRule' that applies to a given piece.
pieceRule :: Piece -> ActionRule
pieceRule piece = case pieceType piece of
    Pawn -> concatActionRules
        [ pawnStep color
        , pawnLeap color
        , pawnCapture color left
        , pawnCapture color right
        , enPassant color left
        , enPassant color right
        ]
    Rook ->
        concatActionRules
            $  map lineOfSightMoveAction gridLines
            ++ map lineOfSightCapture    gridLines
    Knight ->
        concatActionRules
            $  map jumpMove    knightOffsets
            ++ map jumpCapture knightOffsets
    Bishop ->
        concatActionRules
            $  map lineOfSightMoveAction diagonalLines
            ++ map lineOfSightCapture    diagonalLines
    Queen ->
        concatActionRules
            $  map lineOfSightMoveAction queenLines
            ++ map lineOfSightCapture    queenLines
    King ->
        concatActionRules
            $  map jumpMove    queenLines
            ++ map jumpCapture queenLines
            ++ [castleActionRule left, castleActionRule right]
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

concatActionRules :: [ActionRule] -> ActionRule
concatActionRules rules brd pos = concatMap (\rule -> rule brd pos) rules

nthAction :: Int -> ActionRule -> ActionRule
nthAction n rule = fmap (take 1 . drop (n - 1)) . rule

onlyWhen :: (Piece -> Bool) -> MoveRule -> MoveRule
onlyWhen pred rule brd pos = do
    guard $ maybe False pred (brd ! pos)
    rule brd pos

updateWith :: (Piece -> Piece) -> MoveRule -> MoveRule
updateWith pieceUpdater rule = fmap (map moveUpdater) . rule
    where moveUpdater move = move { updater = pieceUpdater . updater move }

withSideEffect :: Move -> Move -> Move
withSideEffect effect move = move { sideEffect = Just effect }

moveFrom :: BoardIx -> BoardIx -> Move
moveFrom start end = Move { movesFrom  = start
                          , movesTo    = end
                          , updater    = \piece -> piece { hasMoved = True }
                          , sideEffect = Nothing
                          }

captureFrom :: BoardIx -> BoardIx -> Action
captureFrom start end = Capture end $ moveFrom start end

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

lineOfSightMoveAction :: (Int, Int) -> ActionRule
lineOfSightMoveAction = noCaptures . lineOfSightMove

lineOfSightCapture :: (Int, Int) -> ActionRule
lineOfSightCapture (dx, dy) brd (sx, sy) =
    map (captureFrom (sx, sy))
        . take 1
        . dropWhile (emptySquareOn brd)
        . takeWhile validSquare
        $ [ (sx + n * dx, sy + n * dy) | n <- [1 ..] ]

jumpMove :: (Int, Int) -> ActionRule
jumpMove = nthAction 1 . noCaptures . lineOfSightMove

jumpCapture :: (Int, Int) -> ActionRule
jumpCapture = nthAction 1 . lineOfSightCapture

pawnDirection :: Color -> (Int, Int)
pawnDirection White = (0, 1)
pawnDirection Black = (0, -1)

pawnStep :: Color -> ActionRule
pawnStep = nthAction 1 . noCaptures . lineOfSightMove . pawnDirection

pawnLeap :: Color -> ActionRule
pawnLeap =
    nthAction 2
        . noCaptures
        . onlyWhen (not . hasMoved)
        . updateWith setPassantTarget
        . lineOfSightMove
        . pawnDirection
    where setPassantTarget piece = piece { enPassantTarget = True }

pawnCapture :: Color -> Int -> ActionRule
pawnCapture color dx = jumpCapture (directionx + dx, directiony)
    where (directionx, directiony) = pawnDirection color

enPassant :: Color -> Int -> ActionRule
enPassant color dx brd (sx, sy) = do
    guard canPass
    return action
  where
    (directionx, directiony) = pawnDirection color
    target                   = (sx + dx, sy)
    end                      = (sx + dx + directionx, sy + directiony)
    canPass                  = validSquare end && validSquare target && maybe
        False
        enPassantTarget
        (brd ! target)
    action = Capture target $ moveFrom (sx, sy) end

castleActionRule :: Int -> ActionRule
castleActionRule = noCaptures . castleMoveRule

castleMoveRule :: Int -> MoveRule
castleMoveRule kingDirX brd (kingX, kingY) = do
    guard . not $ kingMoved || rookMoved || piecesBlocking || movesThroughCheck
    return castleMove
  where
    kingMoved = hasMoved kingPiece
    rookMoved = case rookSquare of
        Just piece -> hasMoved piece || pieceType piece /= Rook
        Nothing    -> True
    piecesBlocking    = any (isJust . (brd !)) betweenSquares
    movesThroughCheck = any (squareThreatenedBy enemy brd) kingTravelSquares
    castleMove        = withSideEffect rookMove kingMove
    (rookX, rookY)    = last $ takeWhile
        validSquare
        [ (kingX + n * kingDirX, kingY) | n <- [1 ..] ]
    betweenSquares    = [ (x, kingY) | x <- rangeExclusive kingX rookX ]
    kingTravelSquares = [ (x, kingY) | x <- rangeInclusive kingX kingTargetX ]
    kingMove          = moveFrom (kingX, kingY) (kingTargetX, kingY)
    rookMove          = moveFrom (rookX, kingY) (rookTargetX, kingY)
    rookTargetX       = kingX + kingDirX
    kingTargetX       = rookTargetX + kingDirX
    kingPiece         = fromJust $ brd ! (kingX, kingY)
    rookSquare        = brd ! (rookX, rookY)
    enemy             = nextTurn . pieceColor $ kingPiece

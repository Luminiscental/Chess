{-|
Module      : Chess.Engine.Moves
Description : Functions for enumerating and working with moves or captures.

This module defines functions for working with possible moves or actions in a chess game, along
with the rules for what moves are possible for given pieces and for checking which moves are legal
to make at a given point.
-}
module Chess.Engine.Moves
    ( applyMove
    , applyAction
    , runAction
    , squareThreatenedBy
    , existsCheckAgainst
    , checkToAddress
    , availableActions
    , actionsForColor
    , actionsForColorUnchecked
    , pieceRule
    )
where

import           Chess.Types
import           Chess.Util
import           Chess.Engine.State             ( stepGame
                                                , nextTurn
                                                )

import qualified Data.Char                     as Char
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

-- | Check if a given 'Action' is a capture.
isCapture :: Action -> Bool
isCapture (Capture _ _) = True
isCapture (NoCapture _) = False

-- | Apply a 'Move' to a 'Board'.
applyMove :: Move -> Board -> Board
applyMove move brd = postMove $ brd // changes
  where
    piece            = movingPiece move
    postMove         = maybe id applyMove $ sideEffect move
    newPassantTarget = setsPassantTarget move || enPassantTarget piece
    oldType          = pieceType piece
    newPiece         = piece { hasMoved = True
                             , pieceType = fromMaybe oldType (promotion move)
                             , enPassantTarget = newPassantTarget
                             }
    changes = [(movesFrom move, Nothing), (movesTo move, Just newPiece)]

-- | Apply an 'Action' to a 'Board'.
applyAction :: Action -> Board -> Board
applyAction (NoCapture move ) board = applyMove move board
applyAction (Capture at move) board = applyMove move $ board // [(at, Nothing)]

-- | Run an action to step a 'Game'.
runAction :: Action -> Game -> Game
runAction action game = stepGame newBoard resetClock game
  where
    newBoard   = applyAction action oldBoard
    resetClock = case action of
        Capture _ _    -> True
        NoCapture move -> isPawn $ oldBoard ! movesFrom move
    isPawn   = maybe False $ (== Pawn) . pieceType
    oldBoard = board game

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
actionsForColor color brd =
    map updateThreat . filter noCheck $ actionsForColorUnchecked color brd
  where
    enemy = nextTurn color
    noCheck action = not . existsCheckAgainst color $ applyAction action brd
    updateThreat action =
        let afterBrd  = applyAction action brd
            check     = existsCheckAgainst enemy afterBrd
            checkmate = check && (null . actionsForColor enemy $ afterBrd)
        in  flip updateMove action $ \move -> move
                { threat = if checkmate
                               then Just Checkmate
                               else if check then Just Check else Nothing
                }

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
    Pawn -> checkPawnPromotion $ concatActionRules
        [ pawnStep color
        , pawnLeap color
        , pawnCapture color left
        , pawnCapture color right
        , enPassant color left
        , enPassant color right
        ]
    Rook ->
        concatActionRules
            $  map lineOfSightNoCapture gridLines
            ++ map lineOfSightCapture   gridLines
    Knight ->
        concatActionRules
            $  map jumpNoCapture knightOffsets
            ++ map jumpCapture   knightOffsets
    Bishop ->
        concatActionRules
            $  map lineOfSightNoCapture diagonalLines
            ++ map lineOfSightCapture   diagonalLines
    Queen ->
        concatActionRules
            $  map lineOfSightNoCapture queenLines
            ++ map lineOfSightCapture   queenLines
    King ->
        concatActionRules
            $  map jumpNoCapture queenLines
            ++ map jumpCapture   queenLines
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
    diagonalLines = [ (dirx, diry) | dirx <- [-1, 1], diry <- [-1, 1] ]
    knightOffsets =
        concat
            [ [(dirx, 2 * diry), (2 * dirx, diry)]
            | (dirx, diry) <- diagonalLines
            ]
    queenLines = gridLines ++ diagonalLines

concatActionRules :: [ActionRule] -> ActionRule
concatActionRules rules brd pos = concatMap (\rule -> rule brd pos) rules

nthAction :: Int -> ActionRule -> ActionRule
nthAction n rule = fmap (take 1 . drop (n - 1)) . rule

onlyWhen :: (Piece -> Bool) -> MoveRule -> MoveRule
onlyWhen pred rule brd pos = do
    guard $ maybe False pred (brd ! pos)
    rule brd pos

updateMove :: (Move -> Move) -> Action -> Action
updateMove updater (NoCapture move ) = NoCapture $ updater move
updateMove updater (Capture at move) = Capture at $ updater move

withSideEffect :: Move -> Move -> Move
withSideEffect effect move = move { sideEffect = Just effect }

moveFrom :: Board -> BoardIx -> BoardIx -> Move
moveFrom board start end = Move { movingPiece       = fromJust $ board ! start
                                , movesFrom         = start
                                , movesTo           = end
                                , promotion         = Nothing
                                , setsPassantTarget = False
                                , sideEffect        = Nothing
                                , threat            = Nothing
                                }

captureFrom :: Board -> BoardIx -> BoardIx -> Action
captureFrom board start end = Capture end $ moveFrom board start end

noCaptures :: MoveRule -> ActionRule
noCaptures moveRule board start = map NoCapture $ moveRule board start

validSquare :: BoardIx -> Bool
validSquare = inRange boardRange

emptySquareOn :: Board -> BoardIx -> Bool
emptySquareOn brd pos = isNothing $ brd ! pos

lineOfSight :: (Int, Int) -> BoardIx -> [BoardIx]
lineOfSight (dx, dy) (sx, sy) =
    takeWhile validSquare [ (sx + n * dx, sy + n * dy) | n <- [1 ..] ]

checkEnemy :: Board -> BoardIx -> BoardIx -> Bool
checkEnemy brd start at = thatColor == Just enemyColor
  where
    myColor    = pieceColor . fromJust $ brd ! start
    enemyColor = nextTurn myColor
    thatColor  = pieceColor <$> brd ! at

lineOfSightMove :: (Int, Int) -> MoveRule
lineOfSightMove offset brd start =
    map (moveFrom brd start) . takeWhile (emptySquareOn brd) $ lineOfSight
        offset
        start

lineOfSightNoCapture :: (Int, Int) -> ActionRule
lineOfSightNoCapture = noCaptures . lineOfSightMove

lineOfSightCapture :: (Int, Int) -> ActionRule
lineOfSightCapture offset brd start =
    map (captureFrom brd start)
        . filter (checkEnemy brd start)
        . take 1
        . dropWhile (emptySquareOn brd)
        $ lineOfSight offset start

jumpNoCapture :: (Int, Int) -> ActionRule
jumpNoCapture = nthAction 1 . lineOfSightNoCapture

jumpCapture :: (Int, Int) -> ActionRule
jumpCapture offset brd start =
    map (captureFrom brd start)
        . filter (checkEnemy brd start)
        . take 1
        $ lineOfSight offset start

pawnDirection :: Color -> (Int, Int)
pawnDirection White = (0, 1)
pawnDirection Black = (0, -1)

pawnStep :: Color -> ActionRule
pawnStep = nthAction 1 . noCaptures . lineOfSightMove . pawnDirection

pawnLeap :: Color -> ActionRule
pawnLeap =
    nthAction 2
        . withPassantTarget
        . noCaptures
        . onlyWhen (not . hasMoved)
        . lineOfSightMove
        . pawnDirection
  where
    withPassantTarget rule = fmap (map $ updateMove setPassant) . rule
    setPassant move = move { setsPassantTarget = True }

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
    action = Capture target $ moveFrom brd (sx, sy) end

checkPawnPromotion :: ActionRule -> ActionRule
checkPawnPromotion rule board pos = do
    action <- rule board pos
    let (targetFile, targetRank) = movesTo (getMove action)
    if targetRank == endRank then promotionsFor action else return action
  where
    endRank = case color of
        White -> 8
        Black -> 1
    color = pieceColor . fromJust $ board ! pos
    promotionsFor action =
        map (flip updateMove action . promoteTo) [Queen, Knight, Rook, Bishop]
    promoteTo newType move = move { promotion = Just newType }

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
    kingMove          = moveFrom brd (kingX, kingY) (kingTargetX, kingY)
    rookMove          = moveFrom brd (rookX, kingY) (rookTargetX, kingY)
    rookTargetX       = kingX + kingDirX
    kingTargetX       = rookTargetX + kingDirX
    kingPiece         = fromJust $ brd ! (kingX, kingY)
    rookSquare        = brd ! (rookX, rookY)
    enemy             = nextTurn . pieceColor $ kingPiece

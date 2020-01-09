module Chess.Engine.Rules
    ( TieCause(..)
    , GameResult(..)
    , TerminationRule
    , fiftyMoveTie
    , insufficientMaterialTie
    , doubleBishopTie
    , threeFoldRepetitionTie
    , stalemateTie
    , anyTie
    )
where

import           Chess.Util                     ( packString )
import qualified Data.Set                      as Set
import           Data.Maybe                     ( listToMaybe )
import           Chess.Engine.State             ( Game(..)
                                                , Color
                                                , PieceType(..)
                                                , boardFEN
                                                , getMaterial
                                                , getBishopColors
                                                )
import           Chess.Engine.Moves             ( availableMoves )

-- TODO: Implement anyWin and isFinished termination rules.

-- | An enumeration of the possible causes of a tie.
data TieCause = FiftyMoveRule | Stalemate | ThreefoldRepetition | CheckmateImpossible deriving (Show, Eq)

-- | An ADT for possible results of a game.
data GameResult = Win Color | Tie TieCause deriving (Show, Eq)

-- | A 'TerminationRule' determines whether a game has finished, giving the result if so.
type TerminationRule = Game -> Maybe GameResult

-- | Draw if there have been 50 half moves since the last pawn move or capture.
fiftyMoveTie :: TerminationRule
fiftyMoveTie game =
    if halfMoveClock game >= 50 then Just $ Tie FiftyMoveRule else Nothing

-- | Draw if there is an insufficient material imbalance for someone to checkmate.
insufficientMaterialTie :: TerminationRule
insufficientMaterialTie game = if gameMaterial `elem` materialStates
    then Just $ Tie CheckmateImpossible
    else Nothing
  where
    materialStates =
        [ (Set.fromList [King]        , Set.fromList [King])
        , (Set.fromList [King, Bishop], Set.fromList [King])
        , (Set.fromList [King]        , Set.fromList [King, Bishop])
        , (Set.fromList [King, Knight], Set.fromList [King])
        , (Set.fromList [King]        , Set.fromList [King, Knight])
        ]
    gameMaterial = getMaterial . board $ game

-- | Draw if both sides have the same color bishop and no other pieces.
doubleBishopTie :: TerminationRule
doubleBishopTie game = if justBishops && sameColors
    then Just $ Tie CheckmateImpossible
    else Nothing
  where
    gameBoard                      = board game
    (whiteMaterial, blackMaterial) = getMaterial gameBoard
    bishopMaterial                 = Set.fromList [King, Bishop]
    justBishops =
        whiteMaterial == bishopMaterial && blackMaterial == bishopMaterial
    (whiteBishopColors, blackBishopColors) = getBishopColors gameBoard
    sameColors = whiteBishopColors == blackBishopColors

-- | Draw if the same board state is seen three times.
threeFoldRepetitionTie :: TerminationRule
threeFoldRepetitionTie game = if countRepetitions >= 3
    then Just $ Tie ThreefoldRepetition
    else Nothing
  where
    countRepetitions =
        (+ 1) . length . filter (== currFEN) $ prevBoardFENs game
    currFEN = packString . boardFEN . board $ game

-- | Draw if there are no available moves to the current player.
stalemateTie :: TerminationRule
stalemateTie game =
    if null $ availableMoves game then Just $ Tie Stalemate else Nothing

-- | Termination rule that checks for any possible tie.
anyTie :: TerminationRule
anyTie game = mapM ($ game) tieRules >>= listToMaybe
  where
    tieRules =
        [ fiftyMoveTie
        , insufficientMaterialTie
        , doubleBishopTie
        , threeFoldRepetitionTie
        , stalemateTie
        ]

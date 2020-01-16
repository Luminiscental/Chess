{-|
Module      : Chess.Engine.Rules
Description : High-level game termination related rules.

This module defines functions for checking rules related to the end of a chess game, detecting
ties and wins.
-}
module Chess.Engine.Rules
    ( checkmateRule
    , fiftyMoveTie
    , insufficientMaterialTie
    , doubleBishopTie
    , threeFoldRepetitionTie
    , stalemateTie
    , anyTermination
    )
where

import           Chess.Types
import           Chess.Util
import           Chess.Engine.State             ( boardFEN
                                                , getMaterial
                                                , getBishopColors
                                                , nextTurn
                                                )
import           Chess.Engine.Moves             ( availableActions
                                                , checkToAddress
                                                )

import qualified Data.Set                      as Set
import           Data.Maybe                     ( listToMaybe )
import           Data.Foldable                  ( asum )

-- | The enemy wins if the current player is in check and cannot move out of it.
checkmateRule :: TerminationRule
checkmateRule game = if checkToAddress game && null (availableActions game)
    then Just $ Win enemy
    else Nothing
    where enemy = nextTurn (toMove game)

-- | Draw if there have been 50 half moves since the last pawn move or capture.
fiftyMoveTie :: TerminationRule
fiftyMoveTie game =
    if halfMoveClock game >= 50 then Just $ Tie FiftyMoveRule else Nothing

-- | Draw if there is an insufficient material imbalance for someone to checkmate.
insufficientMaterialTie :: TerminationRule
insufficientMaterialTie game = if gameMaterial `elem` materialStates
    then Just $ Tie InsufficientMaterial
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
    then Just $ Tie InsufficientMaterial
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
    if null (availableActions game) then Just $ Tie Stalemate else Nothing

-- | Termination rule that checks for any tie or win.
anyTermination :: TerminationRule
anyTermination game = asum $ map ($ game) rules
  where
    rules =
        -- NOTE: Order matters; look for checkmate before ties
        [ checkmateRule
        , fiftyMoveTie
        , insufficientMaterialTie
        , doubleBishopTie
        , threeFoldRepetitionTie
        , stalemateTie
        ]

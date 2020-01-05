module Chess.Engine.Rules
    ()
where

import           Control.Monad                  ( guard )
import qualified Data.Set                      as Set
import           Chess.Engine.State             ( Game
                                                , Color
                                                , PieceType(..)
                                                , board
                                                , halfMoveClock
                                                , getMaterial
                                                , getBishopColors
                                                )

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

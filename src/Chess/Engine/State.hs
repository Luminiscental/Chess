{-|
Module      : Chess.Engine.State
Description : Game state definitions.

This module defines functions for manipulating and observing the state of the board, pieces,
overall game, and so on.
-}
module Chess.Engine.State
    ( nextTurn
    , stepGame
    , makeGame
    , startGame
    )
where

import           Chess.Types
import           Chess.Util
import           Chess.Interface.FEN            ( boardFEN )

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import qualified Data.ByteString               as ByteString
import           Data.ByteString                ( ByteString )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Maybe                     ( catMaybes
                                                , isNothing
                                                )
import           Data.Array.IArray              ( Array
                                                , elems
                                                , assocs
                                                , range
                                                , (//)
                                                , (!)
                                                )

-- | Get the color whose turn is next given the color that moved.
nextTurn :: Color -> Color
nextTurn White = Black
nextTurn Black = White

-- | Reset en passant captures against a color.
resetEnPassant :: Color -> Board -> Board
resetEnPassant color = (fmap . fmap) resetPiece
  where
    resetPiece piece = if pieceColor piece == color
        then piece { enPassantTarget = False }
        else piece

-- | Create a game given starting board and the player with first turn.
makeGame :: Board -> Color -> Game
makeGame brd firstMove = Game { board         = brd
                              , toMove        = firstMove
                              , halfMoveClock = 0
                              , fullMoveCount = 1
                              , prevBoardFENs = []
                              }

-- | An initial game state for normal chess.
startGame :: Game
startGame = makeGame defaultBoard White

-- | Update the game given the new board state after a move.
stepGame
    :: Board -- ^ New board state
    -> Bool -- ^ Whether a capture / pawn move occured
    -> Game -- ^ Initial game state
    -> Game -- ^ New game state
stepGame newBoard resetHalfMoveClock oldGame = Game
    { board         = resetEnPassant nextPlayer newBoard
    , toMove        = nextPlayer
    , halfMoveClock = if resetHalfMoveClock
                          then 0
                          else halfMoveClock oldGame + 1
    , fullMoveCount = case movePlayer of
                          White -> oldFullMoveCount
                          Black -> oldFullMoveCount + 1
    , prevBoardFENs = (packString . boardFEN . board $ oldGame)
                          : prevBoardFENs oldGame
    }
  where
    movePlayer       = toMove oldGame
    nextPlayer       = nextTurn movePlayer
    oldFullMoveCount = fullMoveCount oldGame

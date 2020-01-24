{-|
Module      : Chess.Engine.State
Description : Game state definitions.

This module defines functions for manipulating and observing the state of the board, pieces,
overall game, and so on.
-}
module Chess.Engine.State
    ( squareColor
    , nextTurn
    , stepGame
    , makeGame
    , startGame
    , boardRange
    , emptyBoard
    , defaultBoard
    , getMaterial
    , getBishopColors
    )
where

import           Chess.Types
import           Chess.Util
import           Chess.Interface.Notation       ( boardFEN )

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import           Data.Maybe                     ( catMaybes
                                                , isNothing
                                                )
import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
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

-- | The range of valid chess board indices.
boardRange :: (BoardIx, BoardIx)
boardRange = ((1, 1), (8, 8))

-- | Get the color of a square on the board, used for describing bishop colors.
squareColor :: BoardIx -> Color
squareColor (column, row) =
    if (column + row) `mod` 2 == 0 then Black else White

-- | A board with no pieces on it.
emptyBoard :: Board
emptyBoard = mkArray (const Nothing) boardRange

-- | A list of piece types for the pawn row in an initial board state.
pawns :: [PieceType]
pawns = [ Pawn | _ <- [1 .. 8] ]

-- | A list of piece types for the piece row in an initial board state.
starterPieces :: [PieceType]
starterPieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- | A board setup ready to play a normal chess game.
defaultBoard :: Board
defaultBoard =
    emptyBoard // (blackPieces ++ whitePieces ++ blackPawns ++ whitePawns)
  where
    starters color row typeList =
        [ ( (col, row)
          , Just Piece { pieceType       = pieceType
                       , pieceColor      = color
                       , hasMoved        = False
                       , enPassantTarget = False
                       }
          )
        | (col, pieceType) <- zip [1 ..] typeList
        ]
    blackPieces = starters Black 8 starterPieces
    whitePieces = starters White 1 starterPieces
    blackPawns  = starters Black 7 pawns
    whitePawns  = starters White 2 pawns

-- | Reset en passant captures against a color.
resetEnPassant :: Color -> Board -> Board
resetEnPassant color = (fmap . fmap) resetPiece
  where
    resetPiece piece = if pieceColor piece == color
        then piece { enPassantTarget = False }
        else piece

-- | Return the material on board, as a tuple of white material and black material.
getMaterial :: Board -> (Set PieceType, Set PieceType)
getMaterial brd =
    ( Set.fromList . map pieceType $ whitePieces
    , Set.fromList . map pieceType $ blackPieces
    )
  where
    (whitePieces, blackPieces) =
        List.partition ((== White) . pieceColor) allPieces
    allPieces = catMaybes . elems $ brd

-- | Return the colors of all the bishops owned by white and all the bishops owned by black.
getBishopColors :: Board -> (Set Color, Set Color)
getBishopColors brd =
    (Set.fromList whiteBishopColors, Set.fromList blackBishopColors)
  where
    whiteBishopColors =
        [ squareColor ix | (ix, mPiece) <- assocs brd, isBishopOf White mPiece ]
    blackBishopColors =
        [ squareColor ix | (ix, mPiece) <- assocs brd, isBishopOf Black mPiece ]
    isBishopOf color = maybe
        False
        (\piece -> pieceColor piece == color && pieceType piece == Bishop)

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

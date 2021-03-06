{-|
Module      : Chess.Types
Description : Various type definitions for the library.

This module contains type definitions for use in various other modules, extracted into one
internal module in order to avoid cyclic dependency issues. Also includes basic structure
inspecting functions like 'getMove'.
-}
module Chess.Types
    (
    -- * Types for Chess.Engine.State
    --
    -- | Various data structures for holding the state of a chess game:
      Board(..)
    , BoardIx(..)
    , boardRange
    , emptyBoard
    , defaultBoard
    , Piece(..)
    , PieceType(..)
    , Color(..)
    , Game(..)
    -- * Types for Chess.Engine.Moves
    --
    -- | Various data structures to represent actions or moves in a chess game:
    , BoardSide(..)
    , Move(..)
    , Action(..)
    , Threat(..)
    , getMove
    , captures
    , MoveRule(..)
    , ActionRule(..)
    -- * Types for Chess.Engine.Rules
    --
    -- | Various data structures to represent higher level rules about wins/draws of games:
    , TieCause(..)
    , GameResult(..)
    , TerminationRule(..)
    -- * Types for Chess.Interface.Notation
    --
    -- | Some small utility types for intermediate representations when generating notation.
    , VerboseSAN(..)
    )
where

import           Chess.Util

import           Data.Array.IArray              ( Array
                                                , (//)
                                                )
import           Data.ByteString                ( ByteString )

-- | The board is represented as an array of 'Maybe' 'Piece'.
type Board = Array BoardIx (Maybe Piece)

-- | The range of valid chess board indices.
boardRange :: (BoardIx, BoardIx)
boardRange = ((1, 1), (8, 8))

-- | The board is indexed by @(column, row)@ pairs, from @(1, 1)@ to @(8, 8)@ inclusive. A board
-- index is then a representation of a square on the board, e.g. a3 is @(1, 3)@.
type BoardIx = (Int, Int)

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

-- | Each piece stores its type and color along with whether it has moved and whether it is a
-- possible target for capturing en passant.
data Piece = Piece { pieceType :: PieceType, pieceColor :: Color, hasMoved :: Bool, enPassantTarget :: Bool }
    deriving (Show, Eq)

-- | An enumeration of the different types of 'Piece'.
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq, Ord)

-- | An enumeration of the two player colors.
data Color = Black | White
    deriving (Show, Eq, Ord)

-- | The full game state, including board state, which player's turn is next, the number of half
-- moves since the last capture or pawn advance, the full move count, and a record of previously
-- seen board states encoded as FEN notation.
data Game = Game { board :: Board, toMove :: Color, halfMoveClock :: Int, fullMoveCount :: Int, prevBoardFENs :: [ByteString] }
    deriving (Show, Eq)

-- | Represents possible threats a move could give, used for notation purposes.
data Threat = Check | Checkmate
    deriving (Show, Eq)

-- | A 'BoardSide' is used for castling rights, distinguishing the two horizontal halves of the
-- board.
data BoardSide = Kingside | Queenside
    deriving (Show, Eq)

-- | A 'Move' record contains the start and end locations of a move, including any side effect
-- moves for castling, and an updating function to apply to the moved piece.
data Move = Move { movingPiece :: Piece
                 , movesFrom :: BoardIx
                 , movesTo :: BoardIx
                 , promotion :: Maybe PieceType
                 , setsPassantTarget :: Bool
                 , sideEffect :: Maybe Move
                 , threat :: Maybe Threat}
    deriving (Show, Eq)

-- | An 'Action' represents a 'Move' with metadata about where and whether a capture occurs.
data Action = NoCapture Move | Capture BoardIx Move
    deriving (Show, Eq)

-- | Get the underlying 'Move' for an 'Action'.
getMove :: Action -> Move
getMove (NoCapture move) = move
getMove (Capture _ move) = move

-- | Check if an action is a capture.
captures :: Action -> Bool
captures (Capture _ _) = True
captures (NoCapture _) = False

-- | A 'MoveRule' generates a list of possible moves from a given position on the board.
type MoveRule = Board -> BoardIx -> [Move]

-- | An 'ActionRule' generates a list of possible actions from a given position on the board.
type ActionRule = Board -> BoardIx -> [Action]

-- | An enumeration of the possible causes of a tie.
data TieCause = FiftyMoveRule | Stalemate | ThreefoldRepetition | InsufficientMaterial
    deriving (Show, Eq)

-- | An ADT for possible results of a game.
data GameResult = Win Color | Tie TieCause
    deriving (Show, Eq)

-- | A 'TerminationRule' determines whether a game has finished, giving the result if so.
type TerminationRule = Game -> Maybe GameResult

-- | Contains a verbose move description, which can be simplified down to SAN.
data VerboseSAN = VerboseSAN { pieceNote :: String
                             , startFile :: Int
                             , startRank :: Int
                             , captureNote :: String
                             , targetNote :: String
                             , threatNote :: String
                             , promotionNote :: String
                             , castleNote :: String }

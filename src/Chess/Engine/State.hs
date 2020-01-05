module Chess.Engine.State
    ( Board
    , BoardIx
    , Game(..)
    , Piece(..)
    , PieceType(..)
    , Color(..)
    , nextTurn
    , stepGame
    , startGame
    , boardRange
    , emptyBoard
    , defaultBoard
    , getMaterial
    , getBishopColors
    )
where

import           Data.Maybe                     ( catMaybes )
import qualified Data.List                     as List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Data.Array.IArray              ( Ix
                                                , Array
                                                , elems
                                                , assocs
                                                , array
                                                , range
                                                , (//)
                                                )

-- TODO: PGN, FEN, EPD import/export functions

-- | Utility function to make an array given a function from index to value.
mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray mkElem ixRange =
    array ixRange [ (ix, mkElem ix) | ix <- range ixRange ]

-- | The board is represented as an array of 'Maybe Piece'.
type Board = Array BoardIx (Maybe Piece)
-- | The board is indexed by '(column, row)' pairs, from (1, 1) to (8, 8) inclusive.
type BoardIx = (Int, Int)
-- | Each piece stores its type and color along with whether it has moved.
data Piece = Piece { pieceType :: PieceType, pieceColor :: Color, hasMoved :: Bool }
    deriving (Show, Eq)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Show, Eq, Ord)
data Color = Black | White
    deriving (Show, Eq, Ord)

-- TODO: Some bit-compression function for board state to handle threefold repetition.

-- | The full game state, including board state, which player's turn is next, the number of half
-- moves since the last capture or pawn advance, and the full move count.
data Game = Game { board :: Board, toMove :: Color, halfMoveClock :: Int, fullMoveCount :: Int }

-- | Get the color whose turn is next given the color that moved.
nextTurn :: Color -> Color
nextTurn White = Black
nextTurn Black = White

-- | The range of the chess board indices, for use in functions like 'array'.
boardRange :: (BoardIx, BoardIx)
boardRange = ((1, 1), (8, 8))

-- | Get the color of a square on the board, used for describing bishop colors.
squareColor :: BoardIx -> Color
squareColor (column, row) = if column + row `mod` 2 == 0 then Black else White

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
          , Just Piece { pieceType  = pieceType
                       , pieceColor = color
                       , hasMoved   = False
                       }
          )
        | (col, pieceType) <- zip [1 ..] typeList
        ]
    blackPieces = starters Black 8 starterPieces
    whitePieces = starters White 1 starterPieces
    blackPawns  = starters Black 7 pawns
    whitePawns  = starters White 2 pawns

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

-- | An initial game state for normal chess.
startGame :: Game
startGame = Game { board         = defaultBoard
                 , toMove        = White
                 , halfMoveClock = 0
                 , fullMoveCount = 0
                 }

stepGame
    :: Game -- ^ Initial game state
    -> Board -- ^ New board state
    -> Bool -- ^ Whether a capture / pawn move occured
    -> Game -- ^ New game state
stepGame oldGame newBoard resetHalfMoveClock = Game
    { board         = newBoard
    , toMove        = nextTurn movePlayer
    , halfMoveClock = if resetHalfMoveClock
                          then 0
                          else halfMoveClock oldGame + 1
    , fullMoveCount = case movePlayer of
                          White -> oldFullMoveCount
                          Black -> oldFullMoveCount + 1
    }
  where
    movePlayer       = toMove oldGame
    oldFullMoveCount = fullMoveCount oldGame

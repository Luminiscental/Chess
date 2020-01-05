module Chess.Engine.State
    ( Board
    , BoardIx
    , Piece(..)
    , PieceType(..)
    , PieceColor(..)
    , boardRange
    , emptyBoard
    , defaultBoard
    )
where

import           Data.Array.IArray              ( Ix
                                                , Array
                                                , array
                                                , range
                                                , (//)
                                                )

-- | Utility function to make an array given a function from index to value.
mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray mkElem ixRange =
    array ixRange [ (ix, mkElem ix) | ix <- range ixRange ]

-- | The board is represented as an array of 'Maybe Piece'.
type Board = Array BoardIx (Maybe Piece)
-- | The board is indexed by '(Int, Int)' pairs, from (1, 1) to (8, 8) inclusive.
type BoardIx = (Int, Int)
-- | Each piece stores its type and color along with whether it has moved.
data Piece = Piece { pieceType :: PieceType, pieceColor :: PieceColor, hasMoved :: Bool }
    deriving (Show, Eq)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Show, Eq)
data PieceColor = Black | White
    deriving (Show, Eq)

-- | The range of the chess board indices, for use in functions like 'array'.
boardRange :: (BoardIx, BoardIx)
boardRange = ((1, 1), (8, 8))

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

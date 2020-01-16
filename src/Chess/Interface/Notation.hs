{-|
Module      : Chess.Interface.Notation
Description : Notation export functions.

This module defines functions to generate string representations (FEN, SAN, e.t.c.) from parts of
the game state.
-}
module Chess.Interface.Notation
    ( squareSAN
    , pieceTypeChar
    , pieceFEN
    , boardFEN
    , actionsSAN
    )
where

import           Chess.Types
import           Chess.Util

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Maybe                     ( isNothing )
import           Data.Array.IArray              ( (!)
                                                , range
                                                )

-- TODO: Full PGN, FEN, EPD import/export functions

-- | Get the standard notation for a square on the board.
squareSAN :: BoardIx -> String
squareSAN (x, y) = [Char.chr (Char.ord 'a' + x - 1), Char.intToDigit y]

-- | Get the character representing a given piece type (lower case).
pieceTypeChar :: PieceType -> Char
pieceTypeChar Pawn   = 'p'
pieceTypeChar Rook   = 'r'
pieceTypeChar Knight = 'n'
pieceTypeChar Bishop = 'b'
pieceTypeChar Queen  = 'q'
pieceTypeChar King   = 'k'

-- | Get the FEN notation for a piece.
pieceFEN :: Piece -> Char
pieceFEN piece = if pieceColor piece == White
    then Char.toUpper . pieceTypeChar . pieceType $ piece
    else pieceTypeChar . pieceType $ piece

-- | Get the FEN notation for a given board state, only including piece placements, not the
-- additional metadata.
boardFEN :: Board -> String
boardFEN brd = List.intercalate "/" rows
  where
    rows =
        [ displayRow . map (brd !) $ range ((1, row), (8, row))
        | row <- reverse [1 .. 8]
        ]
    displayRow []                  = ""
    displayRow (Just piece : rest) = pieceFEN piece : displayRow rest
    displayRow (Nothing    : rest) = Char.intToDigit blankCount : afterBlanks
      where
        (empties, afterEmpties) = span isNothing rest
        blankCount              = 1 + length empties
        afterBlanks             = displayRow afterEmpties

-- TODO
-- | Get the algebraic notation for each 'Action' in a list, disambiguating within the list.
actionsSAN :: [Action] -> [String]
actionsSAN actions = []

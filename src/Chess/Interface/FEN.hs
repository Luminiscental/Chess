{-|
Module      : Chess.Interface.FEN
Description : Functions for exporting FEN notation of boards / positions.

This module defines functions to get the FEN notation of pieces, board and game states.
-}
module Chess.Interface.FEN
    ( pieceFEN
    , boardFEN
    , exportFEN
    , parseFEN
    )
where

import           Chess.Types
import           Chess.Util
import           Chess.Interface.SAN            ( squareSAN
                                                , pieceTypeChar
                                                )
import           Chess.Engine.Metrics           ( castlingRightsFor )

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Maybe                     ( isNothing )
import           Data.Array.IArray              ( (!)
                                                , range
                                                , assocs
                                                )
import           Text.Parsec                    ( Parsec )

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

-- | Get the FEN notation for a given position.
exportFEN :: Game -> String
exportFEN game = unwords
    [ piecePlacement
    , activeColor
    , castlingRights
    , passantTarget
    , show $ halfMoveClock game
    , show $ fullMoveCount game
    ]
  where
    brd            = board game
    piecePlacement = boardFEN brd
    activeColor    = map Char.toLower . take 1 . show . toMove $ game
    castlingRights = case whiteCastles ++ blackCastles of
        []  -> "-"
        str -> str
    whiteCastles = map (head . show) $ castlingRightsFor White brd
    blackCastles =
        map (Char.toLower . head . show) $ castlingRightsFor Black brd
    passantTarget =
        case
                map fst
                . filter (maybe False enPassantTarget . snd)
                . assocs
                $ brd
            of
                []          -> "-"
                [(file, 4)] -> squareSAN (file, 3)
                [(file, 5)] -> squareSAN (file, 6)

-- | Parse a 'Game' state from its FEN notation. This loses information about previous positions,
-- so a threefold repetition may be missed as a result of using this.
parseFEN :: Parsec String () Game
parseFEN = undefined

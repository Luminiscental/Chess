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
    , getSANs
    , getVerboseSAN
    , simplifyVerboseSANs
    )
where

import           Chess.Types
import           Chess.Util

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Function                  ( on )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           Data.Array.IArray              ( (!)
                                                , range
                                                )

-- TODO: Full PGN, FEN, EPD import/export functions

fileChar :: Int -> Char
fileChar = Char.chr . (+ (Char.ord 'a' - 1))

rankChar :: Int -> Char
rankChar = Char.intToDigit

-- | Get the standard notation for a square on the board.
squareSAN :: BoardIx -> String
squareSAN (file, rank) = [fileChar file, rankChar rank]

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

-- | Get the standard algebraic notation for each 'Action' in a list, disambiguating within the
-- given list only.
getSANs :: [Action] -> [String]
getSANs = simplifyVerboseSANs . map getVerboseSAN

-- TODO: Append check/checkmate symbols?

-- | Get the 'VerboseSAN' for a given action.
getVerboseSAN :: Action -> VerboseSAN
getVerboseSAN action =
    let
        move                   = getMove action
        isCapture              = captures action
        piece                  = movingPiece move
        (startFile, startRank) = movesFrom move
        pieceNote              = case pieceType piece of
            Pawn  -> if isCapture then [fileChar startFile] else ""
            other -> [Char.toUpper . pieceTypeChar $ other]
        captureNote  = if isCapture then "x" else ""
        targetNote   = squareSAN . movesTo $ move
        updatedPiece = updater move piece
        updatedPieceNote =
            [Char.toUpper . pieceTypeChar . pieceType $ updatedPiece]
        castleNote = if isJust (sideEffect move)
            then case fileChar . fst . movesTo $ move of
                'c' -> "O-O-O"
                'g' -> "O-O"
            else ""
    in
        VerboseSAN { pieceNote        = pieceNote
                   , startFile        = startFile
                   , startRank        = startRank
                   , captureNote      = captureNote
                   , targetNote       = targetNote
                   , updatedPieceNote = updatedPieceNote
                   , castleNote       = castleNote
                   }

-- | Simplify a list of 'VerboseSAN's, mapping to each canonical SAN as a string and disambiguating
-- within the given list.
simplifyVerboseSANs :: [VerboseSAN] -> [String]
simplifyVerboseSANs verbose =
    map castleNote castleSANs
        ++ disambiguate
               [ (equatePieceTarget        , specify (const ""))
               , (equatePieceTargetFile    , specify file)
               , (equatePieceTargetRank    , specify rank)
               , (equatePieceTargetRankFile, specify square)
               , (defaultCase              , showPromotion)
               ]
               nonCastleSANs
  where
    (nonCastleSANs, castleSANs) = List.partition (null . castleNote) verbose
    equatePieceTarget           = (==) `on` (,) <$> pieceNote <*> targetNote
    equatePieceTargetFile =
        (==) `on` (,,) <$> pieceNote <*> targetNote <*> startFile
    equatePieceTargetRank =
        (==) `on` (,,) <$> pieceNote <*> targetNote <*> startRank
    equatePieceTargetRankFile =
        (==) `on` (,,,) <$> pieceNote <*> targetNote <*> startFile <*> startRank

    defaultCase = const . const $ False
    showPromotion san = specify square san ++ updatedPieceNote san

    specify fn san = pieceNote san ++ fn san ++ otherNotes san
    file   = return . fileChar . startFile
    rank   = return . rankChar . startRank
    square = squareSAN . startSquare
    startSquare san = (startFile san, startRank san)
    otherNotes san = captureNote san ++ targetNote san

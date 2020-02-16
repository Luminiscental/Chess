{-|
Module      : Chess.Interface.SAN
Description : Functions for getting SAN notation of objects.

This moduel defines functions to get the SAN notation for pieces, squares and moves.
-}
module Chess.Interface.SAN
    ( pieceTypeChar
    , squareSAN
    , getSANs
    , getVerboseSAN
    , simplifyVerboseSANs
    , parseSAN
    , parseSquare
    )
where

import           Chess.Types
import           Chess.Util

import qualified Data.Char                     as Char
import           Data.Maybe                     ( isJust
                                                , maybeToList
                                                )
import           Data.Function                  ( on )
import           Text.Parsec                    ( Parsec
                                                , oneOf
                                                )

-- | Get the lower case character for a piece type.
pieceTypeChar :: PieceType -> Char
pieceTypeChar Pawn   = 'p'
pieceTypeChar Rook   = 'r'
pieceTypeChar Knight = 'n'
pieceTypeChar Bishop = 'b'
pieceTypeChar Queen  = 'q'
pieceTypeChar King   = 'k'

fileChar :: Int -> Char
fileChar = Char.chr . (+ (Char.ord 'a' - 1))

rankChar :: Int -> Char
rankChar = Char.intToDigit

-- | Get the standard notation for a square on the board.
squareSAN :: BoardIx -> String
squareSAN (file, rank) = [fileChar file, rankChar rank]

-- | Get the standard algebraic notation for each 'Action' in a list, disambiguating within the
-- given list only.
getSANs :: [Action] -> [String]
getSANs = simplifyVerboseSANs . map getVerboseSAN

-- | Get the 'VerboseSAN' for a given action.
getVerboseSAN :: Action -> VerboseSAN
getVerboseSAN action =
    let move                   = getMove action
        isCapture              = captures action
        piece                  = movingPiece move
        (startFile, startRank) = movesFrom move
        pieceNote              = case pieceType piece of
            Pawn  -> if isCapture then [fileChar startFile] else ""
            other -> [Char.toUpper . pieceTypeChar $ other]
        captureNote = if isCapture then "x" else ""
        targetNote  = squareSAN . movesTo $ move
        threatSuffix Check     = "+"
        threatSuffix Checkmate = "#"
        threatNote = maybe "" threatSuffix (threat move)
        promotionNote =
                map (Char.toUpper . pieceTypeChar) . maybeToList . promotion $ move
        castleNote = if isJust (sideEffect move)
            then case fileChar . fst . movesTo $ move of
                'c' -> "O-O-O"
                'g' -> "O-O"
            else ""
    in  VerboseSAN { pieceNote     = pieceNote
                   , startFile     = startFile
                   , startRank     = startRank
                   , captureNote   = captureNote
                   , targetNote    = targetNote
                   , threatNote    = threatNote
                   , promotionNote = promotionNote
                   , castleNote    = castleNote
                   }

-- | Simplify a list of 'VerboseSAN's, mapping to each canonical SAN as a string and disambiguating
-- within the given list.
simplifyVerboseSANs :: [VerboseSAN] -> [String]
simplifyVerboseSANs = disambiguate
    [ (singletonCase          , concatOn [pieceNote, genericNotes])
    , (separateCastleMoves    , concatOn [castleNote])
    , (equatePieceTarget      , concatOn [pieceNote, genericNotes])
    , (equatePieceTargetFile  , concatOn [pieceNote, fileNote, genericNotes])
    , (equatePieceTargetRank  , concatOn [pieceNote, rankNote, genericNotes])
    , (equatePieceTargetSquare, concatOn [pieceNote, squareNote, genericNotes])
    , ( defaultCase
      , concatOn [pieceNote, captureNote, targetNote, promotionNote, threatNote]
      )
    ]
  where
    concatOn fs san = concatMap ($ san) fs
    separateCastleMoves = (&&) `on` null . castleNote
    equatePieceTarget   = (==) `on` (,) <$> pieceNote <*> targetNote
    equatePieceTargetFile =
        (==) `on` (,,) <$> pieceNote <*> targetNote <*> startFile
    equatePieceTargetRank =
        (==) `on` (,,) <$> pieceNote <*> targetNote <*> startRank
    equatePieceTargetSquare =
        (==) `on` (,,) <$> pieceNote <*> targetNote <*> getSquare

    singletonCase = const . const $ True
    defaultCase   = const . const $ False

    fileNote      = return . fileChar . startFile
    rankNote      = return . rankChar . startRank
    squareNote    = squareSAN . getSquare
    getSquare     = (,) <$> startFile <*> startRank
    genericNotes  = concatOn [captureNote, targetNote, threatNote]

-- | Parse SAN notation for a move, given a list of 'Action's for each available move in the
-- relevant position. Returns 'Nothing' if no action matches or if the notation is ambiguous.
parseSAN :: [Action] -> String -> Maybe Action
parseSAN actions san =
    let sanPairs        = zip actions (getSANs actions)
        -- TODO: allow over-verbose specification
        matchingActions = map fst . filter ((== san) . snd) $ sanPairs
    in  case matchingActions of
            [action] -> Just action
            _        -> Nothing

-- | Parse SAN notation for a square on the board.
parseSquare :: Parsec String () BoardIx
parseSquare = do
    file <- oneOf "abcdefgh"
    rank <- oneOf "12345678"
    return (1 + Char.ord file - Char.ord 'a', Char.digitToInt rank)

{-|
Module      : Chess.Interface.FEN
Description : Functions for exporting FEN notation of boards / positions.

This module defines functions to get the FEN notation of pieces, board and game states.
-}
module Chess.Interface.FEN
    ( pieceFEN
    , boardFEN
    , exportFEN
    , parsePieceFEN
    , parseBoardFEN
    , parseFEN
    )
where

import           Chess.Types
import           Chess.Util
import           Chess.Interface.SAN            ( squareSAN
                                                , pieceTypeChar
                                                , parseSquare
                                                )
import           Chess.Engine.Metrics           ( castlingRightsFor )

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Maybe                     ( isNothing )
import           Data.Array.IArray              ( (!)
                                                , (//)
                                                , array
                                                , range
                                                , assocs
                                                )
import           Text.Parsec                    ( Parsec
                                                , (<|>)
                                                , sepBy
                                                , char
                                                , many1
                                                , option
                                                , oneOf
                                                )
import           Control.Monad                  ( when )
import           Data.Function                  ( on )

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

-- | Parse a character case-insensitive, deducing a color from the case.
coloredChar :: Char -> Parsec String () Color
coloredChar c = (char lower >> return Black) <|> (char upper >> return White)
  where
    lower = Char.toLower c
    upper = Char.toUpper c

-- | Parse a single piece from its FEN notation.
parsePieceFEN :: Parsec String () Piece
parsePieceFEN = pawn <|> rook <|> knight <|> bishop <|> queen <|> king
  where
    makePiece pType pColor = Piece pType pColor True False
    pawn   = makePiece Pawn <$> coloredChar 'p'
    rook   = makePiece Rook <$> coloredChar 'r'
    knight = makePiece Knight <$> coloredChar 'n'
    bishop = makePiece Bishop <$> coloredChar 'b'
    queen  = makePiece Queen <$> coloredChar 'q'
    king   = makePiece King <$> coloredChar 'k'

-- | Parse a single rank from its FEN notation.
parseRankFEN :: Parsec String () [Maybe Piece]
parseRankFEN = concat <$> many1 chunk
  where
    chunk  = gaps <|> pieces
    pieces = map Just <$> many1 parsePieceFEN
    gaps   = flip replicate Nothing <$> parseCount

-- | Parse a 'Board' from its FEN notation.
parseBoardFEN :: Parsec String () Board
parseBoardFEN = do
    ranks <- parseRankFEN `sepBy` char '/'
    when (length ranks /= 8)
        $ fail ("Expected 8 ranks, got " ++ show (length ranks))
    return $ array
        boardRange
        [ ((column, row), square)
        | (row   , rank  ) <- zip [8, 7 .. 1] ranks
        , (column, square) <- zip [1 .. 8] rank
        ]

-- | Parse the player whose turn is next from a character w or b.
parsePlayer :: Parsec String () Color
parsePlayer = (char 'w' >> return White) <|> (char 'b' >> return Black)

-- | Parse the castling rights section of a FEN position, returning the white and black board sides
-- with castling rights in a tuple.
parseCastlingRights :: Parsec String () ([BoardSide], [BoardSide])
parseCastlingRights = (char '-' >> return ([], [])) <|> do
    whiteKing  <- option [] $ char 'K' >> return [Kingside]
    whiteQueen <- option [] $ char 'Q' >> return [Queenside]
    blackKing  <- option [] $ char 'k' >> return [Kingside]
    blackQueen <- option [] $ char 'q' >> return [Queenside]
    return (whiteKing ++ whiteQueen, blackKing ++ blackQueen)

-- | Calculate the corrected board state, given direct placements along with an optional en passant
-- target position and the castling rights for each player. By default a piece has moved unless the
-- known information implies that it hasn't.
calculateBoard :: Board -> Maybe BoardIx -> ([BoardSide], [BoardSide]) -> Board
calculateBoard placements passantTarget (whiteCastlingRights, blackCastlingRights)
    = updatePawns
        $  placements
        // (  passantPiece
           ++ castlablePieces White whiteCastlingRights
           ++ castlablePieces Black blackCastlingRights
           )
  where
    updatePawns = array boardRange . map updatePawnAt . assocs
    updatePawnAt (ix, piece) = if isPawn piece && startedAt ix piece
        then (ix, flip fmap piece $ \p -> p { hasMoved = False })
        else (ix, piece)
    isPawn = maybe False ((== Pawn) . pieceType)
    startedAt ix piece =
        let startPiece = defaultBoard ! ix
            sameType   = (==) `on` fmap pieceColor
            sameColor  = (==) `on` fmap pieceType
        in  sameType piece startPiece && sameColor piece startPiece
    passantPiece = case passantTarget of
        Just (file, 3) -> [((file, 4), Just $ Piece Pawn White True True)]
        Just (file, 6) -> [((file, 5), Just $ Piece Pawn Black True True)]
        Nothing        -> []
    castlablePieces color rights =
        let rank = case color of
                White -> 1
                Black -> 8
            king          = ((5, rank), Just $ Piece King color False False)
            kingsideRook  = ((8, rank), Just $ Piece Rook color False False)
            queensideRook = ((1, rank), Just $ Piece Rook color False False)
        in  [ king | not $ null whiteCastlingRights ]
                ++ [ kingsideRook | Kingside `elem` rights ]
                ++ [ queensideRook | Queenside `elem` rights ]

-- TODO: Test parsing functions

-- | Parse a 'Game' state from its FEN notation. Because this function cannot know the position
-- history, detection of threefold repetition may be inaccurate.
parseFEN :: Parsec String () Game
parseFEN = do
    placement <- parseBoardFEN
    char ' '
    nextMove <- parsePlayer
    char ' '
    castlingRights <- parseCastlingRights
    char ' '
    passantSquare <- (char '-' >> return Nothing) <|> fmap Just parseSquare
    char ' '
    halfMove <- parseCount
    char ' '
    fullMove <- parseCount
    return $ Game
        { board         = calculateBoard placement passantSquare castlingRights
        , toMove        = nextMove
        , halfMoveClock = halfMove
        , fullMoveCount = fullMove
        , prevBoardFENs = []
        }

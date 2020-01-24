module Chess.Interface.Export
    ( exportFEN
    )
where

import           Chess.Types
import           Chess.Util
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( mapMaybe )
import           Data.Array.IArray              ( assocs )
import           Chess.Interface.Notation       ( boardFEN
                                                , getVerboseSAN
                                                , squareSAN
                                                )
import           Chess.Engine.Moves             ( actionsForColor
                                                , castlingRightsFor
                                                )

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
    activeColor    = case toMove game of
        White -> "w"
        Black -> "b"
    castlingRights = case whiteCastles ++ blackCastles of
        []  -> "-"
        str -> str
    whiteCastles = map (head . show) $ castlingRightsFor White brd
    blackCastles = map (toLower . head . show) $ castlingRightsFor Black brd
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

{-|
Module      : Chess.Engine.Metrics
Description : Functions to calculate metrics of a given position.

This module defines functions to return metrics such as material, bishop colors and castling rights
for a given position.
-}
module Chess.Engine.Metrics
    ( squareColor
    , materialFor
    , bothMaterial
    , bishopColorsFor
    , castlingRightsFor
    )
where

import           Chess.Types
import           Chess.Util

import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Maybe                     ( catMaybes )
import           Data.Array.IArray              ( (!)
                                                , elems
                                                , assocs
                                                )

-- | Get the color of a square on the board, used for describing bishop colors.
squareColor :: BoardIx -> Color
squareColor (column, row) =
    if (column + row) `mod` 2 == 0 then Black else White

-- | Return the material on board for a given color.
materialFor :: Color -> Board -> Set PieceType
materialFor color brd = Set.fromList . map pieceType $ pieces
  where
    pieces    = filter ((== color) . pieceColor) allPieces
    allPieces = catMaybes . elems $ brd

-- | Return the material for white and black in a tuple. 
bothMaterial :: Board -> (Set PieceType, Set PieceType)
bothMaterial = (,) <$> materialFor White <*> materialFor Black

-- | Return the colors of all the bishops owned by a given color.
bishopColorsFor :: Color -> Board -> Set Color
bishopColorsFor color brd = Set.fromList
    [ squareColor ix
    | (ix, piece) <- assocs brd
    , maybe False (isBishopFor color) piece
    ]
  where
    isBishopFor color piece =
        pieceColor piece == color && pieceType piece == Bishop

-- | Returns the sides a given color has castling rights to.
castlingRightsFor :: Color -> Board -> [BoardSide]
castlingRightsFor color brd =
    [ Kingside | rights right ] ++ [ Queenside | rights left ]
  where
    (left, right) = (-1, 1)
    rights dir =
        let kingX     = 5
            rookX     = min 8 $ kingX + 4 * dir
            rank      = max 1 $ 8 * dir
            kingMoved = maybe False hasMoved $ brd ! (kingX, rank)
            rookMoved = maybe False hasMoved $ brd ! (rookX, rank)
        in  not kingMoved && not rookMoved

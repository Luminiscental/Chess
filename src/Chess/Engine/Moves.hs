module Chess.Engine.Moves
    ( Move(..)
    , applyMove
    )
where

import           Chess.Engine.State             ( Board
                                                , BoardIx
                                                )
import           Data.Array.IArray              ( (//)
                                                , (!)
                                                )

-- | A representation of a move solely as the location before and after the move.
data Move = Move { movesFrom :: BoardIx, movesTo :: BoardIx } deriving (Show, Eq)

-- | Apply a move to a board. This clears the square the move starts at and copies its state to the
-- square the move ends at, replacing whatever was there.
applyMove :: Move -> Board -> Board
applyMove move board = board // [(from, Nothing), (to, piece)]
  where
    from  = movesFrom move
    to    = movesTo move
    piece = board ! from

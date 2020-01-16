module Main where

import           Chess.Types
import           Chess.Util
import           Chess.Engine.State             ( startGame )
import           Chess.Engine.Moves             ( runAction
                                                , availableActions
                                                )
import           Chess.Engine.Rules             ( anyTermination )
import           Chess.Interface.Notation       ( getSANs
                                                , pieceFEN
                                                )

import           Data.Foldable                  ( for_ )
import           Data.Array.IArray              ( (!) )

displayBoard :: Game -> IO ()
displayBoard game = do
    let brd = board game
    for_ [8, 7 .. 1] $ \row -> do
        for_ [1 .. 8] $ \col -> putChar . maybe '.' pieceFEN $ brd ! (col, row)
        putChar '\n'

getChosenAction :: Game -> IO Action
getChosenAction game = do
    let actions   = availableActions game
    let notations = getSANs actions
    for_ (zip [1 ..] notations)
        $ \(idx, notation) -> putStrLn (show idx ++ ": " ++ notation)
    idx <- subtract 1 <$> readLn
    return $ actions !! idx

main :: IO ()
main = go startGame
  where
    go game = do
        displayBoard game
        putStrLn ""
        putStrLn $ show (toMove game) ++ " to play:"
        putStrLn ""
        action <- getChosenAction game
        putStrLn ""
        let newGame = runAction action game
        case anyTermination newGame of
            Just result -> putStrLn $ "Game Ended: " ++ show result
            Nothing     -> go newGame

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Set                      as Set
import           Data.Array.IArray              ( (!)
                                                , (//)
                                                )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           Chess.Engine.State             ( Color(..)
                                                , PieceType(..)
                                                , Piece(..)
                                                , Game(..)
                                                , makeGame
                                                , startGame
                                                , stepGame
                                                , emptyBoard
                                                , defaultBoard
                                                , squareColor
                                                , pieceFEN
                                                , boardFEN
                                                , nextTurn
                                                , getMaterial
                                                , getBishopColors
                                                )
import           Chess.Engine.Rules             ( checkmateRule
                                                , insufficientMaterialTie
                                                , doubleBishopTie
                                                , threeFoldRepetitionTie
                                                , stalemateTie
                                                , anyTermination
                                                )
import           Chess.Engine.Moves             ( Move(..)
                                                , Action(..)
                                                , MoveRule
                                                , ActionRule
                                                , applyMove
                                                , applyAction
                                                , runAction
                                                , squareThreatenedBy
                                                , existsCheckAgainst
                                                , availableActions
                                                , actionsForColor
                                                )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stateTests, ruleTests, moveTests]

stateTests :: TestTree
stateTests = testGroup
    "State"
    [ testCase "Next turn" $ nextTurn White @?= Black
    , testCase "Square colors" $ squareColor (3, 4) @?= White
    , testCase "Piece FEN" $ pieceFEN White Knight @?= 'N'
    , testCase "Board FEN"
    $ boardFEN (defaultBoard // [((4, 5), Just $ Piece King Black False False)])
    @?= "rnbqkbnr/pppppppp/8/3k4/8/8/PPPPPPPP/RNBQKBNR"
    , testCase "Board setup" $ defaultBoard ! (5, 1) @?= Just
        (Piece King White False False)
    , testCase "Board material"
    $   getMaterial testBoard
    @?= (Set.fromList [Bishop], Set.fromList [King])
    , testCase "Bishop colors"
    $   getBishopColors testBoard
    @?= (Set.fromList [Black], Set.empty)
    , testCase "Step game"
    $   halfMoveClock (stepGame emptyBoard False startGame)
    @?= 1
    , testCase "Start game" $ toMove startGame @?= White
    , testCase "Full move count" $ fullMoveCount (dummySteps 3 startGame) @?= 1
    ]
  where
    dummySteps n = foldr (.) id $ replicate n (stepGame defaultBoard False)
    testBoard =
        emptyBoard
            // [ ((3, 3), Just $ Piece Bishop White True False)
               , ((4, 7), Just $ Piece King Black True False)
               ]

ruleTests :: TestTree
ruleTests = testGroup
    "Rules"
    [ testCase "Checkmate"
    $  isJust (checkmateRule checkmate)
    @? "Checkmate should be detected"
    , testCase "Insufficient material"
    $  isJust (insufficientMaterialTie materialTie)
    @? "Tie state should be detected"
    , testCase "Double bishop"
    $  isJust (doubleBishopTie bishopTie)
    @? "Tie state should be detected"
    , testCase "Threefold repetition"
    $  isJust (threeFoldRepetitionTie repeatTie)
    @? "Tie state should be detected"
    , testCase "Stalemate"
    $  isJust (stalemateTie stalemate)
    @? "Tie state should be detected"
    , testCase "No tie"
    $  isNothing (anyTermination startGame)
    @? "The game shouldn't be over"
    ]
  where
    checkmate = makeGame
        (  emptyBoard
        // [ ((7, 8), Just $ Piece King Black True False)
           , ((7, 7), Just $ Piece Queen White True False)
           , ((6, 6), Just $ Piece King White True False)
           ]
        )
        Black
    materialTie = makeGame
        (  emptyBoard
        // [ ((2, 3), Just $ Piece King White True False)
           , ((5, 5), Just $ Piece King Black True False)
           , ((7, 1), Just $ Piece Bishop White True False)
           ]
        )
        Black
    bishopTie = makeGame
        (  emptyBoard
        // [ ((4, 8), Just $ Piece King Black True False)
           , ((7, 7), Just $ Piece King White True False)
           , ((4, 1), Just $ Piece Bishop Black True False)
           , ((7, 6), Just $ Piece Bishop White True False)
           ]
        )
        White
    repeatTie =
        stepGame defaultBoard False . stepGame defaultBoard False $ startGame
    stalemate = makeGame
        (  emptyBoard
        // [ ((6, 8), Just $ Piece King Black True False)
           , ((6, 7), Just $ Piece Pawn White True False)
           , ((6, 6), Just $ Piece King White True False)
           , ((1, 2), Just $ Piece Pawn Black True False)
           , ((1, 1), Just $ Piece Pawn White True False)
           ]
        )
        Black

moveTests :: TestTree
moveTests = testGroup
    "Moves"
    [ testCase "Move application"
    $   applyMove
            Move { movesFrom  = (3, 4)
                 , movesTo    = (5, 7)
                 , updater    = \piece -> piece { hasMoved = True }
                 , sideEffect = Nothing
                 }
            (emptyBoard // [((3, 4), Just $ Piece Pawn White False False)])
    @?= (emptyBoard // [((5, 7), Just $ Piece Pawn White True False)])
    , testCase "Action application"
    $   applyAction
            (Capture
                (3, 3)
                Move
                    { movesFrom  = (1, 1)
                    , movesTo    = (1, 2)
                    , updater    =
                        \piece ->
                            piece { hasMoved = True, enPassantTarget = True }
                    , sideEffect = Nothing
                    }
            )
            (  emptyBoard
            // [ ((3, 3), Just $ Piece Rook White False False)
               , ((1, 1), Just $ Piece Pawn Black False False)
               ]
            )
    @?= (emptyBoard // [((1, 2), Just $ Piece Pawn Black True True)])
    , testCase "Side effect"
    $   applyMove
            Move
                { movesFrom  = (7, 6)
                , movesTo    = (4, 4)
                , updater    = id
                , sideEffect = Just Move { movesFrom  = (1, 1)
                                         , movesTo    = (2, 2)
                                         , updater    = id
                                         , sideEffect = Nothing
                                         }
                }
            (  emptyBoard
            // [ ((7, 6), Just $ Piece Pawn White False False)
               , ((1, 1), Just $ Piece King White False False)
               ]
            )
    @?= (  emptyBoard
        // [ ((4, 4), Just $ Piece Pawn White False False)
           , ((2, 2), Just $ Piece King White False False)
           ]
        )
    , testCase "Resetting half move clock"
    $   halfMoveClock
            (runAction
                (NoCapture Move { movesFrom  = (3, 2)
                                , movesTo    = (3, 3)
                                , updater = \piece -> piece { hasMoved = True }
                                , sideEffect = Nothing
                                }
                )
                startGame
            )
    @?= 0
    , testCase "Threatened king"
    $  squareThreatenedBy
           White
           (  emptyBoard
           // [ ((5, 1), Just $ Piece King Black True False)
              , ((5, 2), Just $ Piece King White True False)
              ]
           )
           (5, 1)
    @? "King should be threatened"
    , testCase "Check existence"
    $  existsCheckAgainst
           Black
           (  defaultBoard
           // [ ((4, 7), Nothing)
              , ((2, 5), Just $ Piece Bishop White True False)
              ]
           )
    @? "Black should be in check"
    , testCase "Available actions" $ length (availableActions startGame) @?= 8 + 8 + 4
    , testCase "Castling"
    $   (  length
        .  actionsForColor Black
        $  emptyBoard
        // [ ((5, 8), Just $ Piece King Black False False)
           , ((8, 8), Just $ Piece Rook Black False False)
           ]
        )
    @?= 1
    , testCase "Castling through check"
    $   (  length
        .  actionsForColor White
        $  emptyBoard
        // [ ((5, 1), Just $ Piece King White False False)
           , ((8, 1), Just $ Piece Rook White False False)
           , ((6, 3), Just $ Piece Rook Black True False)
           ]
        )
    @?= 0
    ]

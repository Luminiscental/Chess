import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Set                      as Set
import           Data.Function                  ( on )
import           Data.Array.IArray              ( array
                                                , (!)
                                                , (//)
                                                )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )

import           Chess.Types
import           Chess.Util
import           Chess.Engine.State             ( makeGame
                                                , startGame
                                                , stepGame
                                                , emptyBoard
                                                , defaultBoard
                                                , squareColor
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
import           Chess.Engine.Moves             ( applyMove
                                                , applyAction
                                                , runAction
                                                , squareThreatenedBy
                                                , existsCheckAgainst
                                                , availableActions
                                                , actionsForColor
                                                )
import           Chess.Interface.Notation       ( squareSAN
                                                , pieceFEN
                                                , boardFEN
                                                , getSANs
                                                )

-- TODO: Test getSANs (and in edge cases)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
    "Tests"
    [stateTests, ruleTests, moveTests, utilTests, notationTests]

utilTests :: TestTree
utilTests = testGroup
    "Util"
    [ testCase "Exclusive range symmetry"
    $   rangeExclusive 1 5
    @?= rangeExclusive 5 1
    , testCase "Inclusive range symmetry"
    $   rangeInclusive 3 7
    @?= rangeInclusive 7 3
    , testCase "Exclusive range values" $ rangeExclusive 1 3 @?= [2]
    , testCase "Inclusive range values" $ rangeInclusive 1 100 @?= [1 .. 100]
    , testCase "Disambiguate tuples"
    $   disambiguate [((==) `on` fst, const 0), ((==) `on` snd, snd)]
                     [(1, 3), (2, 4), (2, 5)]
    @?= [0, 4, 5]
    , testCase "Disambiguate integers"
    $   disambiguate [((==), negate), (const (const False), id)]
                     [1, 3, 2, 1, 1]
    @?= [1, -3, -2, 1, 1]
    ]

stateTests :: TestTree
stateTests = testGroup
    "State"
    [ testCase "Next turn" $ nextTurn White @?= Black
    , testCase "Square colors" $ squareColor (3, 4) @?= White
    , testCase "Piece FEN" $ pieceFEN (Piece Knight White False False) @?= 'N'
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
            Move { movingPiece = Piece Pawn White False False
                 , movesFrom   = (3, 4)
                 , movesTo     = (5, 7)
                 , updater     = \piece -> piece { hasMoved = True }
                 , sideEffect  = Nothing
                 }
            (emptyBoard // [((3, 4), Just $ Piece Pawn White False False)])
    @?= (emptyBoard // [((5, 7), Just $ Piece Pawn White True False)])
    , testCase "Action application"
    $   applyAction
            (Capture
                (3, 3)
                Move
                    { movingPiece = Piece Pawn Black False False
                    , movesFrom   = (1, 1)
                    , movesTo     = (1, 2)
                    , updater     =
                        \piece ->
                            piece { hasMoved = True, enPassantTarget = True }
                    , sideEffect  = Nothing
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
                { movingPiece = Piece Pawn White False False
                , movesFrom   = (7, 6)
                , movesTo     = (4, 4)
                , updater     = id
                , sideEffect  = Just Move
                                    { movingPiece = Piece King White False False
                                    , movesFrom   = (1, 1)
                                    , movesTo     = (2, 2)
                                    , updater     = id
                                    , sideEffect  = Nothing
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
                (NoCapture Move { movingPiece = Piece Pawn White False False
                                , movesFrom   = (3, 2)
                                , movesTo     = (3, 3)
                                , updater = \piece -> piece { hasMoved = True }
                                , sideEffect  = Nothing
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
    , testCase "Available actions"
    $   length (availableActions startGame)
    @?= 8
    +   8
    +   4
    , testCase "Castling"
    $   (  length
        .  actionsForColor Black
        $  emptyBoard
        // [ ((5, 8), Just $ Piece King Black False False)
           , ((8, 8), Just $ Piece Rook Black False False)
           ]
        )
    @?= 9
    +   5
    +   1 -- rook moves + king moves + castle move
    , testCase "Castling through check"
    $   (  length
        .  actionsForColor White
        $  emptyBoard
        // [ ((5, 1), Just $ Piece King White False False)
           , ((8, 1), Just $ Piece Rook White False False)
           , ((7, 3), Just $ Piece Rook Black True False)
           ]
        )
    @?= 9
    +   5 -- rook moves + king moves (no castle move)
    , testCase "Center knight"
    $   (  length
        .  actionsForColor White
        $  emptyBoard
        // [((4, 4), Just $ Piece Knight White True False)]
        )
    @?= 8
    , testCase "Corner knight"
    $   (  length
        .  actionsForColor Black
        $  emptyBoard
        // [((1, 1), Just $ Piece Knight Black True False)]
        )
    @?= 2
    , testCase "Pawn promotion"
    $   (  length
        .  actionsForColor White
        $  emptyBoard
        // [((1, 7), Just $ Piece Pawn White True False)]
        )
    @?= 4
    ]

notationTests :: TestTree
notationTests = testGroup
    "Notation"
    [ testCase "Squares" $ squareSAN (3, 5) @?= "c5"
    , testCase "Piece FEN" $ pieceFEN (Piece Knight White False False) @?= 'N'
    , testCase "Board FEN"
    $   boardFEN defaultBoard
    @?= "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    , testCase "Castle SAN"
    $      "O-O"
    `elem` listSANs
               Black
               (  emptyBoard
               // [ ((5, 8), Just $ Piece King Black False False)
                  , ((8, 8), Just $ Piece Rook Black False False)
                  ]
               )
    @?     "Kingside castle SAN should be given"
    , testCase "Pawn move SAN"
    $   listSANs
            White
            (emptyBoard // [((1, 3), Just $ Piece Pawn White True False)])
    @?= ["a4"]
    , testCase "Knight move SAN"
    $   Set.fromList
            (listSANs
                Black
                (emptyBoard // [((8, 8), Just $ Piece Knight Black False False)])
            )
    @?= Set.fromList ["Nf7", "Ng6"]
    , testCase "Move disambiguation"
    $                Set.fromList ["R1a3", "Rdf8", "Qh4e1"]
    `Set.isSubsetOf` Set.fromList
                         (listSANs
                             White
                             (  emptyBoard
                             // [
                             -- Rooks need to specify rank here (R1a3)
                                  ( (1, 1)
                                  , Just $ Piece Rook White False False
                                  )
                                , ( (1, 5)
                                  , Just $ Piece Rook White False False
                                  )
                            -- Rooks need to specify file here (Rdf8)
                                , ( (4, 8)
                                  , Just $ Piece Rook White False False
                                  )
                                , ( (8, 8)
                                  , Just $ Piece Rook White False False
                                  )
                                ,
                            -- Queens need to specify both rank and file here (Qh4e1)
                                  ( (5, 4)
                                  , Just $ Piece Queen White False False
                                  )
                                , ( (8, 4)
                                  , Just $ Piece Queen White False False
                                  )
                                , ( (8, 1)
                                  , Just $ Piece Queen White False False
                                  )
                                ]
                             )
                         )
    @?               "Disambiguated move SANs should be given"
    , testCase "Pawn promotion"
    $   Set.fromList
            (listSANs
                Black
                (emptyBoard // [((5, 2), Just $ Piece Pawn Black True False)])
            )
    @?= Set.fromList ["e1Q", "e1R", "e1N", "e1B"]
    , testCase "En passant notation"
    $      "dxc6"
    `elem` listSANs
               White
               (  emptyBoard
               // [ ((3, 5), Just $ Piece Pawn Black True True)
                  , ((4, 5), Just $ Piece Pawn White True False)
                  ]
               )
    @?     "En passant SAN should be given"
    , testCase "Simple capture"
    $      "Qxe4"
    `elem` listSANs
               Black
               (  emptyBoard
               // [ ((3, 2), Just $ Piece Queen Black True False)
                  , ((5, 4), Just $ Piece Pawn White True True)
                  ]
               )
    @?     "Capture SAN should be given"
    , testCase "Disambiguated capture"
    $      "Qa5xd8"
    `elem` listSANs
               White
               (  emptyBoard
               // [ ((4, 5), Just $ Piece Queen White False False)
                  , ((1, 5), Just $ Piece Queen White False False)
                  , ((1, 8), Just $ Piece Queen White False False)
                  , ((4, 8), Just $ Piece Knight Black False False)
                  ]
               )
    @?     "Disambiguated capture SAN should be given"
    ]
    where listSANs player = getSANs . availableActions . flip makeGame player

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
import           Chess.Engine.Rules             ( insufficientMaterialTie
                                                , doubleBishopTie
                                                , threeFoldRepetitionTie
                                                , stalemateTie
                                                , anyTie
                                                )
import           Chess.Engine.Moves             ( Move(..)
                                                , MoveRule
                                                , applyMove
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
    ]
  where
    testBoard =
        emptyBoard
            // [ ((3, 3), Just $ Piece Bishop White True False)
               , ((4, 7), Just $ Piece King Black True False)
               ]

ruleTests :: TestTree
ruleTests = testGroup
    "Rules"
    [ testCase "Insufficient material"
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
    $  isNothing (anyTie startGame)
    @? "There should be no tie"
    ]
  where
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
           , ((1, 1), Just $ Piece Pawn Black True False)
           , ((1, 2), Just $ Piece Pawn White True False)
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
                   , captures   = Nothing
                   , sideEffect = Nothing
                   }
              (emptyBoard // [((3, 4), Just $ Piece Pawn White False False)])
      @?= (emptyBoard // [((5, 7), Just $ Piece Pawn White True False)])
    ]

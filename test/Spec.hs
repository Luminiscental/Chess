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
                                                , emptyBoard
                                                , defaultBoard
                                                , squareColor
                                                , nextTurn
                                                , getMaterial
                                                , getBishopColors
                                                )
import           Chess.Engine.Rules             ( insufficientMaterialTie
                                                , doubleBishopTie
                                                , anyTie
                                                )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stateTests, ruleTests]

stateTests :: TestTree
stateTests = testGroup
    "State"
    [ testCase "Next turn" $ nextTurn White @?= Black
    , testCase "Square colors" $ squareColor (3, 4) @?= White
    , testCase "Board setup" $ defaultBoard ! (5, 1) @?= Just
        (Piece King White False)
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
            // [ ((3, 3), Just $ Piece Bishop White True)
               , ((4, 7), Just $ Piece King Black True)
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
    , testCase "No tie"
    $  isNothing (anyTie startGame)
    @? "There should be no tie"
    ]
  where
    materialTie = makeGame
        (  emptyBoard
        // [ ((2, 3), Just $ Piece King White True)
           , ((5, 5), Just $ Piece King Black True)
           , ((7, 1), Just $ Piece Bishop White True)
           ]
        )
        Black
    bishopTie = makeGame
        (  emptyBoard
        // [ ((4, 8), Just $ Piece King Black True)
           , ((7, 7), Just $ Piece King White True)
           , ((4, 1), Just $ Piece Bishop Black True)
           , ((7, 6), Just $ Piece Bishop White True)
           ]
        )
        White

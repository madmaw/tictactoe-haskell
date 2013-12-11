module TicTacToe.GameDescription(
        GameDescription(..),
        createGameFromGameDescription
) where

import Data.Map
import Data.Sequence
import System.Random
import System.IO

import TicTacToe.Game(Board(..), Game(..), Player(..), Token(..), GameWinState(..), Mind(..))

data GameDescription = GameDescription {
        boardDiagonal :: Int,
        numberOfPlayers :: Int,
        numberOfHumanPlayers::Int
} deriving Show

createGameFromGameDescription::GameDescription -> Int -> (Board, Map Player Mind)
createGameFromGameDescription (GameDescription boardDiagonal numberOfPlayers numberOfHumanPlayers) rngSeed = do
        let tiles = createTiles boardDiagonal
        let players = createPlayers numberOfPlayers 1
        let minds = createMinds (viewl players) numberOfHumanPlayers (boardDiagonal * boardDiagonal) rngSeed
        let game = Game boardDiagonal boardDiagonal boardDiagonal players
        let board = Board {
                game=game,
                tiles=tiles,
                currentPlayer=(index players 0),
                winState=Undecided
        }
        (board, minds)

createTiles::Int->Seq (Seq (Maybe Token))
createTiles diagonal = Data.Sequence.replicate diagonal (Data.Sequence.replicate diagonal Nothing)

createPlayers::Int->Int->Seq Player
createPlayers 0 _ = Data.Sequence.empty
createPlayers numberOfPlayers playerNumber = do
        let player = Player (show playerNumber)
        let rest = (createPlayers (numberOfPlayers - 1) (playerNumber + 1))
        player <| rest

createMinds::ViewL Player->Int->Int->Int->Map Player Mind
createMinds EmptyL _ _ _ = Data.Map.fromList []
createMinds (player:<xs) numberOfHumanPlayers persistence rngSeed = do
        let map = createMinds (viewl xs) (numberOfHumanPlayers-1) persistence (rngSeed+1)
        let mind = createMind (numberOfHumanPlayers > 0) persistence rngSeed
        insert player mind map

createMind::Bool -> Int -> Int -> Mind
createMind False persistence rngSeed = MindRandom persistence (mkStdGen rngSeed)
createMind True _ _ = MindStream stdin

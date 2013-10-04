module TicTacToe.GameDescription(
	GameDescription(..),
	createGameFromGameDescription
) where

import System.Random

import TicTacToe.Game(Game(..), Player(..), Mark(..), GameWinState(..), MindInstance(..))
import TicTacToe.MindAIRandom(MindAIRandom(..), RandomGenInstance(..))

data GameDescription = GameDescription { 
	boardDiagonal :: Int,
	numberOfPlayers :: Int, 
	numberOfHumanPlayers::Int 
} deriving Show

createGameFromGameDescription::GameDescription -> Game
createGameFromGameDescription (GameDescription boardDiagonal numberOfPlayers numberOfHumanPlayers) = do
	let marks = createMarks boardDiagonal
	let players = createPlayers numberOfPlayers numberOfHumanPlayers 1
	Game boardDiagonal boardDiagonal boardDiagonal marks players (head players) Undecided

createMarks::Int->[[Mark]]
createMarks diagonal = replicate diagonal (replicate diagonal Empty)

createPlayers::Int->Int->Int->[Player]
createPlayers 0 _ _ = []
createPlayers numberOfPlayers numberOfHumanPlayers playerNumber = do
	let player = Player (show playerNumber) (MindInstance (MindAIRandom (RandomGenInstance (mkStdGen playerNumber) ) ) )
	player:(createPlayers (numberOfPlayers - 1) (numberOfHumanPlayers - 1) (playerNumber + 1))





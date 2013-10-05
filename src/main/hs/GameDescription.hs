module TicTacToe.GameDescription(
	GameDescription(..),
	createGameFromGameDescription
) where

import Data.Map
import System.Random
import System.IO

import TicTacToe.Game(Board(..), Game(..), Player(..), Token(..), GameWinState(..), MindInstance(..), Mind(..))
import TicTacToe.MindAIRandom(MindAIRandom(..), RandomGenInstance(..))
import TicTacToe.MindStream(MindStream(..))

data GameDescription = GameDescription { 
	boardDiagonal :: Int,
	numberOfPlayers :: Int, 
	numberOfHumanPlayers::Int 
} deriving Show

createGameFromGameDescription::GameDescription -> Int -> Board
createGameFromGameDescription (GameDescription boardDiagonal numberOfPlayers numberOfHumanPlayers) rngSeed = do
	let tiles = createTiles boardDiagonal
	let players = createPlayers numberOfPlayers 1
	let minds = createMinds players numberOfHumanPlayers (boardDiagonal * boardDiagonal) rngSeed
	let game = Game boardDiagonal boardDiagonal boardDiagonal players 
	Board {
		game=game,
		tiles=tiles,
		currentPlayer=(head players), 
		winState=Undecided,
		minds=minds
	}

createTiles::Int->[[Maybe Token]]
createTiles diagonal = replicate diagonal (replicate diagonal Nothing)

createPlayers::Int->Int->[Player]
createPlayers 0 _ = []
createPlayers numberOfPlayers playerNumber = do
	let player = Player (show playerNumber)
	player:(createPlayers (numberOfPlayers - 1) (playerNumber + 1))

createMinds::[Player]->Int->Int->Int->Map Player MindInstance
createMinds [] _ _ _ = fromList []
createMinds (player:xs) numberOfHumanPlayers persistence rngSeed = do
	let map = createMinds xs (numberOfHumanPlayers-1) persistence (rngSeed+1)
	let mind = createMind (numberOfHumanPlayers > 0) persistence rngSeed
	insert player mind map

createMind::Bool -> Int -> Int -> MindInstance
createMind False persistence rngSeed = MindInstance $ MindAIRandom persistence (RandomGenInstance (mkStdGen rngSeed))
createMind True _ _ = MindInstance $ MindStream stdin
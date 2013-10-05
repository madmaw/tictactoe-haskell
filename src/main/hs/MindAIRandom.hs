module TicTacToe.MindAIRandom(
	MindAIRandom(..),
	RandomGenInstance(..)
) where

import System.Random
import Debug.Trace(trace)
import TicTacToe.Game(Mind(..), Action(..), Token(..), Board(..), Game(..), Player(..))

data RandomGenInstance = forall g. RandomGen g => RandomGenInstance g
instance Show RandomGenInstance where
	show (RandomGenInstance g) = "RNG"

data MindAIRandom = MindAIRandom {
	persistence::Int,
	randomGenerator::RandomGenInstance
} deriving Show

instance Mind MindAIRandom where
	think (MindAIRandom persistence rngInstance) player board = do
		case rngInstance of 
			RandomGenInstance rng -> do
				let (rng', action) = findFreeAction rng board player persistence
				return (MindAIRandom persistence (RandomGenInstance rng'), action)
		--return (mind, AddToken (Claim player) (0, 0))

findFreeAction::(RandomGen g)=> g -> Board -> Player -> Int -> (g, Action)
findFreeAction rng _ _ 0 = (rng, Pass)
findFreeAction rng board@(Board{tiles = tiles, game=(Game{boardWidth = boardWidth, boardHeight = boardHeight})}) player persistenceRemaining = do
	let (xr, rng') = next rng
	let (yr, rng'') = next rng'
	let x = xr `mod` boardWidth
	let y = yr `mod` boardHeight 
	let tile = trace ((show xr) ++ "/" ++ (show yr)) (tiles!!x!!y)
	case tile of
		Nothing -> (rng'', AddToken (Claim player) (x, y)) 
		Just _ -> findFreeAction rng'' board player (persistenceRemaining-1)

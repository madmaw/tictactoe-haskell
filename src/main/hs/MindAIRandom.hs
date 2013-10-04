module TicTacToe.MindAIRandom(
	MindAIRandom(..),
	RandomGenInstance(..)
) where

import System.Random
import TicTacToe.Game(Mind(..), Move(..))

data RandomGenInstance = forall g. RandomGen g => RandomGenInstance g
instance Show RandomGenInstance where
	show (RandomGenInstance g) = "RNG"

data MindAIRandom = MindAIRandom {
	randomGenerator::RandomGenInstance
} deriving Show

instance Mind MindAIRandom where
	think mind game = return (mind, PlaceToken 0 0)
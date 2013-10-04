module TicTacToe.Game(
	Mark(..),
	Move(..),
	Player(..),
	Mind(..),
	MindInstance(..),
	Game(..),
	GameWinState(..)
) where	

import Data.Maybe

data Move = PlaceToken {
	x::Integer,
	y::Integer
} | Pass deriving Show

class (Show a) => Mind a where 
	think :: a -> Game -> IO((a, Move))


data MindInstance = forall m. Mind m => MindInstance m 
instance Show MindInstance where
	show (MindInstance m) = show m

data Player = Player {
	name::String, 
	mind::MindInstance
} deriving Show



data Mark = 
	Token {
		owner::Player
	} | 
	Obstacle | 
	Empty deriving Show

data GameWinState = 
	Undecided | 
	Draw |
	Won { 
		winner::Player
	} deriving Show

data Game = Game {
	boardWidth::Int,
	boardHeight::Int,
	minimumSequenceLength::Int,
	marks::[[Mark]],
	players::[Player], 
	currentPlayer::Player,
	winningPlayer::GameWinState
} deriving Show

--act::Game -> Move -> ActionResult






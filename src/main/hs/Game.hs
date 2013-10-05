module TicTacToe.Game(
	Token(..),
	Action(..),
	Player(..),
	Mind(..),
	MindInstance(..),
	Board(..),
	Game(..),
	GameWinState(..)
) where	

import Data.Maybe
import Data.Map

data Action = AddToken {
	addToken::Token,
	addAt::(Int, Int)
} | Pass | Fail String deriving Show

class (Show a) => Mind a where 
	think :: a -> Player -> Board -> IO((a, Action))


data MindInstance = forall m. Mind m => MindInstance m 
instance Show MindInstance where
	show (MindInstance m) = show m

data Player = Player {
	name::String
} deriving (Show, Ord, Eq)

data Token = 
	Claim {
		forPlayer::Player
	} | 
	Obstacle deriving Show

data GameWinState = 
	Undecided | 
	Draw |
	Won { 
		winningPlayer::Player
	} deriving Show

data Game = Game {
	boardWidth::Int,
	boardHeight::Int,
	minimumSequenceLength::Int,
	players::[Player]
} deriving Show

data Board = Board {
	game::Game,
	tiles::[[Maybe Token]],
	currentPlayer::Player,
	winState::GameWinState,
	minds::Map Player MindInstance
} deriving Show

data BoardTransition = 
	TokenAddedToBoard {
		addedAt::(Int, Int),
		addedToken::Token
	} | 
	Passed


data ActionResult = 
	InvalidAction Action | 
	Success [(Board, BoardTransition)]

act::Board -> Action -> ActionResult
act Board{game=game, tiles=tiles, currentPlayer=currentPlayer, winState=winState, minds=minds} (AddToken token at) = do
	let tiles' = tiles
	let currentPlayer' = currentPlayer
	let winState' = winState
	let minds' = minds
	let board' = Board{
		game = game,
		tiles = tiles',
		currentPlayer = currentPlayer',
		winState = winState', 
		minds = minds'
	}
	Success [(board', TokenAddedToBoard{addedAt=at, addedToken=token})]

act board Pass = Success [(board, Passed)]
act _ action = InvalidAction action









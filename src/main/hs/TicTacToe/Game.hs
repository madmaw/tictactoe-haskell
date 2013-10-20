module TicTacToe.Game(
        Token(..),
        Action(..),
        Player(..),
        Mind(..),
        MindInstance(..),
        Board(..),
        Game(..),
        GameWinState(..), 
        ActionResult(..),
        act
) where        

import Data.Maybe
import Data.Map
import Data.Sequence

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
        players::Seq Player
} deriving Show

data Board = Board {
        game::Game,
        tiles::Seq (Seq (Maybe Token)),
        currentPlayer::Player,
        winState::GameWinState
} deriving Show

data BoardTransition = 
        TokenAddedToBoard {
                addedAt::(Int, Int),
                addedToken::Token
        } | 
        Passed deriving Show


data ActionResult = 
        InvalidAction Action | 
        Success [(Board, BoardTransition)]

act::Board -> Action -> ActionResult
act board Pass = Success [(board, Passed)]
act Board{game=game, tiles=tiles, currentPlayer=currentPlayer} action@AddToken{addAt=addAt, addToken=addToken} = do
        -- set tile at x,y
        let (x, y) = addAt
        let tile = index (index tiles y) x
        case tile of
                Nothing -> do
                        let Game{players=players} = game
                        let tilesY = index tiles y
                        let tilesY' = Data.Sequence.update x (Just addToken) tilesY
                        let tiles' = Data.Sequence.update y tilesY' tiles
                        let (Just currentPlayerIndex) = elemIndexL currentPlayer players
                        let currentPlayerIndex' = (currentPlayerIndex + 1) `mod` (Data.Sequence.length players)
                        let currentPlayer' = index players currentPlayerIndex'
                        let winState' = calculateWinState tiles'
                        -- TODO set new mind state
                        let board' = Board{
                                game = game,
                                tiles = tiles',
                                currentPlayer = currentPlayer',
                                winState = winState'
                        }
                        Success[(board', TokenAddedToBoard{addedAt=addAt, addedToken=addToken})]
                otherwise -> do
                        InvalidAction action
act _ action = InvalidAction action

calculateWinState::Seq (Seq (Maybe Token)) -> GameWinState
calculateWinState tiles = Undecided










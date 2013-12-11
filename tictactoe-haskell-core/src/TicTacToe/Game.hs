module TicTacToe.Game(
        Token(..),
        Action(..),
        Player(..),
        Mind(..),
        Board(..),
        BoardTransition(..),
        Game(..),
        GameWinState(..),
        ActionResult(..),
        act
) where

import Debug.Trace(trace)
import Data.Maybe
import Data.Map
import Data.Sequence
import Data.List;
import System.Random
import System.IO(Handle, hGetLine);

data Action = AddToken {
        addToken::Token,
        addAt::(Int, Int)
} | Pass | Fail String deriving Show

data Mind =
    MindRandom {
        persistence::Int,
        randomGenerator::StdGen
    } |
    MindStream {
        input::Handle
    } deriving Show



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
                        let winState' = calculateWinState game tiles'
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

calculateWinState::Game -> Seq (Seq (Maybe Token)) -> GameWinState
calculateWinState Game{boardWidth=boardWidth, boardHeight=boardHeight, minimumSequenceLength=minimumSequenceLength} tiles = do
        let verticalTestPoints = cartesianProduct [0..boardWidth-1] [0..(boardHeight - minimumSequenceLength)]
        let horizontalTestPoints = cartesianProduct [0..(boardWidth - minimumSequenceLength)] [0..boardHeight-1]
        let forwardDiagonalTestPoints = cartesianProduct [0..(boardWidth - minimumSequenceLength)] [0..(boardHeight - minimumSequenceLength)]
        let backwardDiagonalTestPoints = cartesianProduct [minimumSequenceLength-1..boardWidth-1] [0..(boardHeight - minimumSequenceLength)]
        let positionsAndDeltas = mergeToPairs verticalTestPoints (0,1) ++ mergeToPairs horizontalTestPoints (1,0) ++ mergeToPairs forwardDiagonalTestPoints (1,1) ++ mergeToPairs backwardDiagonalTestPoints (-1, 1)

        -- partially complete function
        let results = trace (show (verticalTestPoints, horizontalTestPoints, forwardDiagonalTestPoints, backwardDiagonalTestPoints)) Prelude.map (calculateSequenceWinState minimumSequenceLength tiles Nothing) positionsAndDeltas

        -- filter to first non-undecided value
        let result = trace (show results) Data.List.foldr decide Undecided results
        result

decide::GameWinState -> GameWinState -> GameWinState
decide Undecided b = b
decide a _ = a

calculateSequenceWinState::Int -> Seq (Seq (Maybe Token)) -> Maybe GameWinState -> ((Int, Int), (Int, Int)) -> GameWinState
calculateSequenceWinState 0 _ Nothing _ = Undecided
calculateSequenceWinState 0 _ (Just Undecided) _ = Undecided
calculateSequenceWinState 0 _ (Just winStateSoFar) _ = winStateSoFar
calculateSequenceWinState remainingSequenceLength tiles maybeWinState ((x, y), (dx, dy)) = do
        let tile = trace (show (x,y, remainingSequenceLength))  index (index tiles y) x
        let winState = calculateTileWinState tile
        let remainingSequenceLength' = (remainingSequenceLength-1)
        case (maybeWinState, winState) of
            (_, Undecided) -> Undecided
            (Just Undecided, _) ->  Undecided
            (Nothing, Won{}) -> calculateSequenceWinState remainingSequenceLength' tiles (Just winState) ((x+dx, y+dy), (dx, dy))
            (Just Won{winningPlayer=previousWinner}, Won{winningPlayer=currentWinner}) -> calculateSequenceWinState remainingSequenceLength' tiles (Just (calculatePlayerWinState previousWinner currentWinner)) ((x+dx, y+dy), (dx, dy))



calculatePlayerWinState::Player -> Player -> GameWinState
calculatePlayerWinState player1 player2
        | player1 == player2 = Won{winningPlayer=player1}
        | otherwise = Undecided

calculateTileWinState::Maybe Token -> GameWinState
calculateTileWinState (Just Claim{forPlayer=forPlayer}) = Won{winningPlayer=forPlayer}
calculateTileWinState _ = Undecided

cartesianProduct::[a]->[b]->[(a,b)]
-- list comprehension (?)
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

mergeToPairs::[a]->b->[(a,b)]
mergeToPairs [] _ = []
mergeToPairs (x:xs) y = (x,y):mergeToPairs xs y

think::Mind -> Player -> Board -> IO((Mind, Action))
think MindRandom{persistence=persistence, randomGenerator=rng} player board = do
        let (rng', action) = findFreeAction rng board player persistence
        return (MindRandom persistence rng', action)
think mind@MindStream{input=input} player board = do
        line <- hGetLine input
        -- TODO handle exceptions
        let pos = read line
        return (mind, AddToken (Claim player) pos)



findFreeAction::StdGen -> Board -> Player -> Int -> (StdGen, Action)
findFreeAction rng _ _ 0 = (rng, Pass)
findFreeAction rng board@(Board{tiles = tiles, game=(Game{boardWidth = boardWidth, boardHeight = boardHeight})}) player persistenceRemaining = do
        let (xr, rng') = next rng
        let (yr, rng'') = next rng'
        let x = xr `mod` boardWidth
        let y = yr `mod` boardHeight
        let tile = (index (index tiles y) x)
        case tile of
                Nothing -> (rng'', AddToken (Claim player) (x, y))
                Just _ -> findFreeAction rng'' board player (persistenceRemaining-1)





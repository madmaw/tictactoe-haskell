import System.Environment
import Data.Time
import Data.Map
import Data.Maybe
import TicTacToe.GameDescriptionFromArgs
import TicTacToe.GameDescription
import TicTacToe.Game

main::IO()
main = do
	args <- getArgs
	let gameDescription = createGameDescriptionFromArgs args
	(UTCTime _ time) <- getCurrentTime	
	putStrLn $ show gameDescription
	case gameDescription of 
		TicTacToe.GameDescriptionFromArgs.Success gameDescription -> do
			let (board, minds) = createGameFromGameDescription gameDescription (floor $ toRational time)
			play board minds
		otherwise -> 
			putStrLn $ show gameDescription 

play::Board -> Map Player MindInstance -> IO()
play board@Board{currentPlayer=currentPlayer} minds = do
	putStrLn $ show board
	let (Just mindInstance) = Data.Map.lookup currentPlayer minds
	case mindInstance of
		MindInstance mind -> do 
			(mind', action) <- think mind currentPlayer board
			let minds' = insert currentPlayer (MindInstance mind') minds
			let actionResult = act board action
			case actionResult of 
				TicTacToe.Game.InvalidAction action -> do
					putStrLn $ "invalid "++(show action)
					play board minds'
				TicTacToe.Game.Success boardTransitions -> do
					putStrLn $ show boardTransitions
					let (board'@Board{winState=winState'}, _) = last boardTransitions
					case winState' of 
						Won{winningPlayer=winningPlayer} -> putStrLn $ "winner! "++(show winningPlayer)
						otherwise -> play board' minds'

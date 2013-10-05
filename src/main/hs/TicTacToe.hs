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
	displayGameDescription gameDescription 


displayGameDescription::GameDescriptionFromArgs -> IO()
displayGameDescription (Success gameDescription) = do
	(UTCTime _ time) <- getCurrentTime	
	let board@Board{game=game, currentPlayer=currentPlayer, minds=minds} = createGameFromGameDescription gameDescription (floor $ toRational time)
	let (Just mindInstance) = Data.Map.lookup currentPlayer minds
	--let MindInstance mind = mindInstance
	case mindInstance of
		MindInstance mind -> do 
			putStrLn $ show board
			putStrLn $ show mindInstance
			(mind', action) <- think mind currentPlayer board
			putStrLn $ show mind'
			putStrLn $ show action
displayGameDescription ErrorDiagonalNotNumber = putStrLn $ "board diagonal must be a number"
displayGameDescription ErrorNumberOfPlayersNotNumber = putStrLn $ "number of players must be a number"
displayGameDescription (ErrorUnrecognizedArgument arg) = putStrLn $ "unrecognized argument \""++arg++"\""
displayGameDescription a = putStrLn $ show a


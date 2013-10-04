import System.Environment
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
	let game = createGameFromGameDescription gameDescription
	putStrLn $ show game
displayGameDescription ErrorDiagonalNotNumber = putStrLn $ "board diagonal must be a number"
displayGameDescription ErrorNumberOfPlayersNotNumber = putStrLn $ "number of players must be a number"
displayGameDescription (ErrorUnrecognizedArgument arg) = putStrLn $ "unrecognized argument \""++arg++"\""
displayGameDescription a = putStrLn $ show a


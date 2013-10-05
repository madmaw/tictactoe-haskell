module TicTacToe.GameDescriptionFromArgs
(
	GameDescriptionFromArgs(..),
	createGameDescriptionFromArgs
) where

import TicTacToe.GameDescription(GameDescription(..))

data GameDescriptionFromArgs = 
	Success GameDescription | 
	ErrorDiagonalNotNumber | 
	ErrorNumberOfPlayersNotNumber | 
	ErrorAtLeastOnePlayerRequired |
	ErrorUnrecognizedArgument String deriving Show

createGameDescriptionFromArgs::[String] -> GameDescriptionFromArgs
createGameDescriptionFromArgs = createGameDescriptionWithDefaultFromArgs $ GameDescription 3 2 1 

createGameDescriptionWithDefaultFromArgs::GameDescription ->[String] -> GameDescriptionFromArgs
createGameDescriptionWithDefaultFromArgs g [] = Success g
createGameDescriptionWithDefaultFromArgs (GameDescription _ numberOfPlayers numberOfHumanPlayers) ("-d":diagonalString:xs) = do
	let diagonalMaybe = maybeRead diagonalString
	case diagonalMaybe of
		Just diagonal -> createGameDescriptionWithDefaultFromArgs (GameDescription diagonal numberOfPlayers numberOfHumanPlayers) xs
		Nothing -> ErrorDiagonalNotNumber
createGameDescriptionWithDefaultFromArgs defaultGameDescription ("-p":numberOfPlayersString:xs) = do
	let numberOfPlayersMaybe = maybeRead numberOfPlayersString
	createGameDescriptionWithDefaultFromNumberOfPlayersString defaultGameDescription numberOfPlayersMaybe xs
createGameDescriptionWithDefaultFromArgs _ (a:_) = ErrorUnrecognizedArgument a

createGameDescriptionWithDefaultFromNumberOfPlayersString::GameDescription -> Maybe Int -> [String] -> GameDescriptionFromArgs
createGameDescriptionWithDefaultFromNumberOfPlayersString (GameDescription diagonal _ numberOfHumanPlayers) (Just numberOfPlayers) xs
	| numberOfPlayers < 0  	= ErrorAtLeastOnePlayerRequired
	| otherwise				= createGameDescriptionWithDefaultFromArgs (GameDescription diagonal numberOfPlayers numberOfHumanPlayers) xs
createGameDescriptionWithDefaultFromNumberOfPlayersString _ Nothing _ = ErrorNumberOfPlayersNotNumber

maybeRead::Read a => String -> Maybe a
maybeRead s = maybeReads (reads s) s

maybeReads::Read a => [(a, String)] -> String -> Maybe a
maybeReads [] _ = Nothing
maybeReads ((v,s):[]) _ = Just v
maybeReads _ o = fail $ "multiple interpretations "++o
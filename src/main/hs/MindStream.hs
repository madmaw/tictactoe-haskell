module TicTacToe.MindStream(
	MindStream(..)
) where

import System.IO(Handle, hGetLine);
import TicTacToe.Game(Mind(..), Action(..), Token(..), Game(..), Player(..))

data MindStream = MindStream {
	input::Handle
} deriving Show

instance Mind MindStream where
	think mind@(MindStream input) player game = do
		line <- hGetLine input
		-- TODO handle exceptions
		let pos = read line
		return (mind, AddToken (Claim player) pos)

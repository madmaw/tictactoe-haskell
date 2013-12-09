module TicTacToe.CLI.RenderToConsole (
    renderTransitionsToConsole,
    renderBoardToConsole
) where

import Debug.Trace(trace)
import Data.Sequence
import TicTacToe.Game


renderTransitionsToConsole::[(Board, BoardTransition)]->IO()
renderTransitionsToConsole [] = return ()
renderTransitionsToConsole (t:[]) = renderTransitionToConsole t
renderTransitionsToConsole ((_, transition):xs) = do
    putStrLn $ show transition
    renderTransitionsToConsole xs

renderTransitionToConsole::(Board, BoardTransition)->IO()
renderTransitionToConsole (board, transition) = do
    putStrLn $ show transition
    renderBoardToConsole board 0 0

renderBoardToConsole::Board->Int->Int->IO()
renderBoardToConsole board@Board{game=game, tiles=tiles} x y = do
    let height = Data.Sequence.length tiles

    if (y < height)
        then do
            let row = index tiles y
            let width = Data.Sequence.length row
            if (x == 0)
                then if (y == 0)
                    then do
                        -- put all the numbers
                        let line = foldl (\acc s -> acc ++ " " ++ s) " " (map (show) [0..width-1])
                        putStrLn line
                        putStrLn ""
                    else do
                        let line = iterate (\acc -> acc ++ "+-") "  -"
                        putStrLn $ line !! (width-1)
                else return ()
	
            if (x < width)
                then do
                    if (x == 0)
                        then do
                            putStr $ (show y)++" "
                        else putChar '|'
                    let tile = index row x
                    let char = tileToChar game tile
                    putChar char
                    renderBoardToConsole board (x+1) y
                else do
                    putStrLn " "
                    renderBoardToConsole board 0 (y+1)
        else return ()



tileToChar::Game->Maybe Token->Char
tileToChar Game{players=players} tile = do
    case tile of
        Just (Claim{forPlayer=player})-> do
            let (Just index) = elemIndexL player players
            case index of
                0 -> 'O'
                otherwise -> 'X'
        Just Obstacle -> '#'
        Nothing -> ' '

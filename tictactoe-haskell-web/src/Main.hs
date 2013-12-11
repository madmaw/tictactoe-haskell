import Data.Map
import Data.Maybe
import TicTacToe.GameDescription
import TicTacToe.Game

main::IO()
main = do
    -- need some way of getting a **random** seed!
    let time = 123423
    let gameDescription = GameDescription{boardDiagonal=3, numberOfPlayers=2, numberOfHumanPlayers=1}
    let (board, minds) = createGameFromGameDescription gameDescription time
    let html = renderBoardToSVG 300 300 board
    --Just div <- elemById "ttt"
    --setProp div "innerHTML" html
    -- signals? FRP?
    --div << html
    putStrLn html



renderBoardToSVG::Int->Int->Board->String
renderBoardToSVG width height board = do
    "<svg width=\""
    ++ (show width)
    ++ "\" height=\""
    ++ (show height)
    ++ "\" viewBox=\"0 0 "
    ++ (show width)
    ++ " "
    ++ (show height)
    ++ "\"><rect x=\"0\" y=\"0\" width=\"100\" height=\"100\"/></svg>"



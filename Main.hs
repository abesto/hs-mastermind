module Main where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Model
import System.Random
import Control.Monad (when, liftM)

data BoardPosition = BoardPosition Int Int
data WBoardPosition = WBoardPosition Int Int

data UI = UI {
      uiGame   :: Game
    , uiWBoard :: Window
    , uiStyles :: [CursesStyle]
    }

---- Conversion between coordinate systems ----
-- All systems are 0-based
-- ModelPosition: row 0 is the first guess; can't index the code row (used in Model.hs)
-- BoardPosition: row 0 is the code row; guess rows are indexed top-to-bottom
-- WBoardPosition: the raw character position on the board window
wBoardToBoard :: WBoardPosition -> BoardPosition
wBoardToBoard (WBoardPosition y x) = BoardPosition
                                       ((y + 1) `div` 2 - 1)
                                       ((x + 1) `div` 2 - 1)

boardToWBoard :: BoardPosition -> WBoardPosition
boardToWBoard (BoardPosition y x) = WBoardPosition
                                       (y * 2 + 1)
                                       (x * 2 + 1)

boardToModel :: UI -> BoardPosition -> ModelPosition
boardToModel UI {uiGame=game} (BoardPosition y x) = ModelPosition (rowCount game - y) x

modelToBoard :: UI -> ModelPosition -> BoardPosition
modelToBoard UI {uiGame=game} (ModelPosition y x) = BoardPosition (rowCount game - y) x

screenToModel :: UI -> WBoardPosition -> ModelPosition
screenToModel ui = boardToModel ui . wBoardToBoard

modelToScreen :: UI -> ModelPosition -> WBoardPosition
modelToScreen ui = boardToWBoard . modelToBoard ui


---- Drawing functions ----
putCh :: Char -> UI -> WBoardPosition -> IO ()
putCh c (UI {uiWBoard=wBoard}) (WBoardPosition y x) = mvWAddStr wBoard y x [c]

drawBox :: UI -> (Int, Int) -> (Int, Int) -> IO ()
drawBox ui (y0, x0) (y1, x1) = do
  mapM_ (putCh '|' ui) [WBoardPosition y x | y <- [y0+1 .. y1-1], x <- [x0, x1]]
  mapM_ (putCh '-' ui) [WBoardPosition y x | y <- [y0, y1], x <- [x0+1 .. x1]]
  mapM_ (putCh '+' ui) [WBoardPosition y x | y <- [y0, y1], x <- [x0, x1]]

drawCodePeg :: UI -> CodePeg -> IO ()
drawCodePeg _ Empty = return ()
drawCodePeg UI {uiStyles=styles, uiWBoard=wBoard} p =
    wWithStyle wBoard (styles !! fromEnum p) (wAddStr wBoard $ show $ fromEnum p)

showCode :: UI -> IO ()
showCode ui@(UI {uiGame=game}) = mapM_ f $ zip [0..] (codePegs $ code game)
    where f (n, p) = do
            moveBoard ui $ BoardPosition 0 n
            drawCodePeg ui p

drawBoard :: UI -> IO ()
drawBoard ui@(UI {uiGame=game}) = do
  erase
  mapM_ (uncurry $ drawBox ui) [
                         ((2*row, 2*column), (2*(row+1), 2*(column+1))) |
                         row <- [0 .. rowCount game], column <- [0 .. pegCount game - 1]
                        ]
  mapM_ (putCh '?' ui) [boardToWBoard $ BoardPosition 0 x | x <- [0 .. pegCount game - 1]]

-- Position where the number of black key pegs begins
-- the row parameter is in the BoardPosition coordinate system
blacksPosition :: Game -> Int -> WBoardPosition
blacksPosition game row =
    let f (BoardPosition r c) = BoardPosition r (c+2) in
    boardToWBoard $ f $ BoardPosition row (pegCount game - 1)

whitesPosition :: Game -> Int -> WBoardPosition
whitesPosition game row =
    let f (WBoardPosition r c) = WBoardPosition r (c+10) in
    f $ blacksPosition game row

debug :: Show a => a -> IO ()
debug game = do
  (y, x) <- getYX stdScr
  move 40 0
  wAddStr stdScr $ show game
  move y x


---- Navigation ----
getYXwBoard :: UI -> IO WBoardPosition
getYXwBoard ui = uncurry WBoardPosition `liftM` (getYX $ uiWBoard ui)

moveWBoard :: UI -> WBoardPosition -> IO ()
moveWBoard ui (WBoardPosition y x) = wMove (uiWBoard ui) y x

getYXBoard :: UI -> IO BoardPosition
getYXBoard ui = wBoardToBoard `liftM` getYXwBoard ui

moveBoard :: UI -> BoardPosition -> IO ()
moveBoard ui = moveWBoard ui . boardToWBoard

getYXModel :: UI -> IO ModelPosition
getYXModel ui = screenToModel ui `liftM` getYXwBoard ui

moveModel :: UI -> ModelPosition -> IO ()
moveModel ui = moveWBoard ui . modelToScreen ui


---- Handle user input ----
handleInput :: UI -> IO ()
handleInput ui = let sizeX = pegCount (uiGame ui) in do
  --debug game
  wRefresh (uiWBoard ui)
  c <- getCh
  boardPos@(BoardPosition y x) <- getYXBoard ui
  case c of
    KeyLeft  -> do
           when (x > 0) $ moveBoard ui $ BoardPosition y (x-1)
           handleInput ui
    KeyRight -> do
           when (x < (sizeX-1)) $ moveBoard ui $ BoardPosition y (x+1)
           handleInput ui
    KeyChar 'g' ->
        let ui'@(UI {uiGame=game'}) = ui {uiGame = guess (uiGame ui)} in
        do
          when (outcome game' /= Playing) $ do
              showCode ui'
              moveBoard ui boardPos
          when (all (== Empty) (guessPegs $ currentGuess game')) $ do
              moveWBoard ui' $ blacksPosition game' y
              wAddStr (uiWBoard ui) $ show $ length $ filter (== Black) $ keyPegs $ lastResult game'
              moveWBoard ui' $ whitesPosition game' y
              wAddStr (uiWBoard ui) $ show $ length $ filter (== White) $ keyPegs $ lastResult game'
              moveModel ui' $ ModelPosition (length (guesses game') - 1) x
          handleInput ui'
    KeyChar 'q' ->
        return ()
    KeyChar k ->
        let
            f [(i, "")] =
                  let ui'@(UI {uiGame=game'}) = ui {uiGame = modifyGuess (uiGame ui) x (toEnum i)} in do
                    drawCodePeg ui' $ toEnum $ fromEnum $ pegAt game' $ boardToModel ui' boardPos
                    moveBoard ui boardPos
                    handleInput ui'
            f _ = handleInput ui
        in f $ reads [k]
    _ -> handleInput ui


-- Startup
main :: IO ()
main = let rows = 10
           pegs = 4
           colors = 6
           (WBoardPosition boardLines boardColumns) = boardToWBoard $ BoardPosition rows (pegs - 1) in do
  start
  keypad stdScr True
  echo False

  game <- generateGame rows pegs colors `liftM` getStdGen
  styles <- convertStyles [Style c BlackB | c <- [BlackF, RedF, GreenF, BlueF, YellowF, MagentaF, CyanF]]
  wBoard <- newWin (boardLines + 10) (boardColumns + 20) 0 0

  let ui = UI {
             uiGame = game
           , uiWBoard = wBoard
           , uiStyles = styles
           } in do


      drawBoard ui

      let (WBoardPosition y x) = blacksPosition game 0 in
        mvWAddStr (uiWBoard ui) y x "Blacks"

      let (WBoardPosition y x) = whitesPosition game 0 in
        mvWAddStr (uiWBoard ui) y x "Whites"

      moveBoard ui $ modelToBoard ui (ModelPosition 0 0)
      refresh
      handleInput ui

      echo True
      end

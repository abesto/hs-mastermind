module Main where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Model
import System.Random
import Control.Monad (when, liftM)

data BoardPosition = BoardPosition Int Int
data ScreenPosition = ScreenPosition Int Int

---- Conversion between coordinate systems ----
-- All systems are 0-based
-- ModelPosition: row 0 is the first guess; can't index the code row (used in Model.hs)
-- BoardPosition: row 0 is the code row; guess rows are indexed top-to-bottom
-- ScreenPosition: the raw character position on screen, as used by ncurses
screenToBoard :: ScreenPosition -> BoardPosition
screenToBoard (ScreenPosition y x) = BoardPosition
                                       ((y + 1) `div` 2 - 1)
                                       ((x + 1) `div` 2 - 1)

boardToScreen :: BoardPosition -> ScreenPosition
boardToScreen (BoardPosition y x) = ScreenPosition
                                       (y * 2 + 1)
                                       (x * 2 + 1)

boardToModel :: Game -> BoardPosition -> ModelPosition
boardToModel game (BoardPosition y x) = ModelPosition (rowCount game - y) x

modelToBoard :: Game -> ModelPosition -> BoardPosition
modelToBoard game (ModelPosition y x) = BoardPosition (rowCount game - y) x

screenToModel :: Game -> ScreenPosition -> ModelPosition
screenToModel game = boardToModel game . screenToBoard

modelToScreen :: Game -> ModelPosition -> ScreenPosition
modelToScreen game = boardToScreen . modelToBoard game


---- Drawing functions ----
putCh :: Char -> ScreenPosition -> IO ()
putCh c (ScreenPosition y x) = mvAddCh y x $ toEnum $ fromEnum c

drawBox :: (Int, Int) -> (Int, Int) -> IO ()
drawBox (y0, x0) (y1, x1) = do
  mapM_ (putCh '|') [ScreenPosition y x | y <- [y0+1 .. y1-1], x <- [x0, x1]]
  mapM_ (putCh '-') [ScreenPosition y x | y <- [y0, y1], x <- [x0+1 .. x1]]
  mapM_ (putCh '+') [ScreenPosition y x | y <- [y0, y1], x <- [x0, x1]]

drawCodePeg :: [CursesStyle] -> CodePeg -> IO ()
drawCodePeg _ Empty = return ()
drawCodePeg styles p = do
  setStyle $ styles !! fromEnum p
  wAddStr stdScr $ show $ fromEnum p
  resetStyle

showCode :: [CursesStyle] -> Game -> IO ()
showCode styles game = mapM_ f $ zip [0..] (codePegs $ code game)
    where f (n, p) = do
            moveBoard $ BoardPosition 0 n
            drawCodePeg styles p

drawBoard :: Game -> IO ()
drawBoard game = do
  erase
  mapM_ (uncurry drawBox) [
                         ((2*row, 2*column), (2*(row+1), 2*(column+1))) |
                         row <- [0 .. rowCount game], column <- [0 .. pegCount game - 1]
                        ]
  mapM_ (putCh '?') [boardToScreen $ BoardPosition 0 x | x <- [0 .. pegCount game - 1]]

-- Position where the number of black key pegs begins
-- the row parameter is in the BoardPosition coordinate system
blacksPosition :: Game -> Int -> ScreenPosition
blacksPosition game row =
    let f (BoardPosition r c) = BoardPosition r (c+2) in
    boardToScreen $ f $ BoardPosition row (pegCount game - 1)

whitesPosition :: Game -> Int -> ScreenPosition
whitesPosition game row =
    let f (ScreenPosition r c) = ScreenPosition r (c+10) in
    f $ blacksPosition game row

debug :: Show a => a -> IO ()
debug game = do
  (y, x) <- getYX stdScr
  move 40 0
  wAddStr stdScr $ show game
  move y x


---- Navigation ----
getYXScreen :: IO ScreenPosition
getYXScreen = do
  (y, x) <- getYX stdScr
  return $ ScreenPosition y x

moveScreen :: ScreenPosition -> IO ()
moveScreen (ScreenPosition y x) = move y x

getYXBoard :: IO BoardPosition
getYXBoard = screenToBoard `liftM` getYXScreen

moveBoard :: BoardPosition -> IO ()
moveBoard = moveScreen . boardToScreen

getYXModel :: Game -> IO ModelPosition
getYXModel game = screenToModel game `liftM` getYXScreen

moveModel :: Game -> ModelPosition -> IO ()
moveModel game = moveScreen . modelToScreen game


---- Handle user input ----
handleInput :: [CursesStyle] -> Game -> IO ()
handleInput styles game = let sizeX = pegCount game in do
  --debug game
  refresh
  c <- getCh
  boardPos@(BoardPosition y x) <- getYXBoard
  case c of
    KeyLeft  -> do
           when (x > 0) $ moveBoard $ BoardPosition y (x-1)
           handleInput styles game
    KeyRight -> do
           when (x < (sizeX-1)) $ moveBoard $ BoardPosition y (x+1)
           handleInput styles game
    KeyChar 'g' ->
        let game' = guess game in
        do
          when (outcome game' /= Playing) $ do
              showCode styles game'
              moveBoard boardPos
          when (all (== Empty) (guessPegs $ currentGuess game')) $ do
              moveScreen $ blacksPosition game' y
              wAddStr stdScr $ show $ length $ filter (== Black) $ keyPegs $ lastResult game'
              moveScreen $ whitesPosition game' y
              wAddStr stdScr $ show $ length $ filter (== White) $ keyPegs $ lastResult game'
              moveModel game' $ ModelPosition (length (guesses game') - 1) x
          handleInput styles game'
    KeyChar 'q' ->
        return ()
    KeyChar k ->
        let
            f [(i, "")] =
                  let game' = modifyGuess game x (toEnum i) in do
                    drawCodePeg styles $ toEnum $ fromEnum $ pegAt game' $ boardToModel game' boardPos
                    moveBoard boardPos
                    handleInput styles game'
            f _ = handleInput styles game
        in f $ reads [k]
    _ -> handleInput styles game


-- Startup
main :: IO ()
main = do
  game <- generateGame 10 4 6 `liftM` getStdGen
  start
  keypad stdScr True
  echo False

  styles <- convertStyles [Style c BlackB | c <- [BlackF, RedF, GreenF, BlueF, YellowF, MagentaF, CyanF]]

  drawBoard game

  moveScreen $ blacksPosition game 0
  wAddStr stdScr "Blacks"

  moveScreen $ whitesPosition game 0
  wAddStr stdScr "Whites"

  moveBoard $ modelToBoard game (ModelPosition 0 0)
  refresh
  handleInput styles game

  echo True
  end

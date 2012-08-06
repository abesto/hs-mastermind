module Main where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Model
import System.Random
import Control.Monad (when, liftM, (>=>))

data BoardPosition = BoardPosition { boardY :: Int, boardX :: Int }
data WBoardPosition = WBoardPosition { wBoardY :: Int, wBoardX :: Int }

data UI = UI {
      uiGame      :: Game
    , uiWOutcome  :: Window
    , uiWBoard    :: Window
    , uiStyles    :: [CursesStyle]
    }

---- Conversion between coordinate systems ----
-- All systems are 0-based
-- ModelPosition: row 0 is the first guess; can't index the code row (used in Model.hs)
-- BoardPosition: row 0 is the code row; guess rows are indexed top-to-bottom
-- WBoardPosition: the raw character position on the board window
wBoardToBoard :: WBoardPosition -> BoardPosition
wBoardToBoard (WBoardPosition y x) = BoardPosition
                                       (y `div` 2 - 1)
                                       (x `div` 2 - 1)

boardToWBoard :: BoardPosition -> WBoardPosition
boardToWBoard (BoardPosition y x) = WBoardPosition
                                       (y * 2 + 2)
                                       (x * 2 + 3)

boardToModel :: UI -> BoardPosition -> ModelPosition
boardToModel UI {uiGame=game} (BoardPosition y x) = ModelPosition (rowCount game - y) x

modelToBoard :: UI -> ModelPosition -> BoardPosition
modelToBoard UI {uiGame=game} (ModelPosition y x) = BoardPosition (rowCount game - y) x

wBoardToModel :: UI -> WBoardPosition -> ModelPosition
wBoardToModel ui = boardToModel ui . wBoardToBoard

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

drawCodePeg :: UI -> Window -> CodePeg -> IO ()
drawCodePeg _ _ Empty = return ()
drawCodePeg UI {uiStyles=styles} window p =
    wWithStyle window (styles !! fromEnum p) (wAddStr window $ show $ fromEnum p)

showCode :: UI -> IO ()
showCode ui@(UI {uiGame=game}) = mapM_ f $ zip [0..] (codePegs $ code game)
    where f (n, p) = do
            moveBoard ui $ BoardPosition 0 n
            drawCodePeg ui (uiWBoard ui) p

drawBoard :: UI -> IO ()
drawBoard ui@(UI {uiGame=game}) = let
    cellPositions = map boardToWBoard [BoardPosition y x | y <- [0 .. rowCount game], x <- [0 .. pegCount game - 1]]
    in do
  mapM_ (uncurry $ drawBox ui) [((y-1, x-1), (y+1, x+1)) | (WBoardPosition y x) <- cellPositions]
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
getYXwBoard ui = uncurry WBoardPosition `liftM` getYX (uiWBoard ui)

moveWBoard :: UI -> WBoardPosition -> IO ()
moveWBoard ui (WBoardPosition y x) = wMove (uiWBoard ui) y x

getYXBoard :: UI -> IO BoardPosition
getYXBoard ui = wBoardToBoard `liftM` getYXwBoard ui

moveBoard :: UI -> BoardPosition -> IO ()
moveBoard ui = moveWBoard ui . boardToWBoard

getYXModel :: UI -> IO ModelPosition
getYXModel ui = wBoardToModel ui `liftM` getYXwBoard ui

moveModel :: UI -> ModelPosition -> IO ()
moveModel ui = moveWBoard ui . modelToScreen ui


---- Handle user input ----
handleInput :: UI -> IO ()
handleInput ui = let sizeX = pegCount (uiGame ui) in do
  --debug $ uiGame ui
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
          when (length (results game') == length (guesses $ uiGame ui)) $ do
              moveWBoard ui' $ blacksPosition game' y
              wAddStr (uiWBoard ui) $ show $ length $ filter (== Black) $ keyPegs $ lastResult game'
              moveWBoard ui' $ whitesPosition game' y
              wAddStr (uiWBoard ui) $ show $ length $ filter (== White) $ keyPegs $ lastResult game'
              moveModel ui' $ ModelPosition (length (guesses game') - 1) x
          if outcome game' == Playing
             then handleInput ui'
             else do
              moveBoard ui' boardPos
              gameOver ui'
    KeyChar 'q' ->
        return ()
    KeyChar k ->
        let
            f [(i, "")] =
                  let ui'@(UI {uiGame=game'}) = ui {uiGame = modifyGuess (uiGame ui) x (toEnum i)} in do
                    drawCodePeg ui' (uiWBoard ui') $ toEnum $ fromEnum $ pegAt game' $ boardToModel ui' boardPos
                    moveBoard ui boardPos
                    handleInput ui'
            f _ = handleInput ui
        in f $ reads [k]
    _ -> handleInput ui


gameOver :: UI -> IO ()
gameOver ui@(UI {uiWOutcome = w, uiWBoard = wb, uiGame = g}) = do
  cursSet CursorInvisible
  showCode ui
  mvWAddStr w 0 0 $ if outcome g == Won then "Congratulations!" else "Oops :("
  mvWAddStr w 1 0 "Press any key to exit"
  wRefresh w
  wRefresh wb
  _ <- getCh
  return ()

-- Startup
main :: IO ()
main = getStdGen >>= \gen -> let
           rows = 10
           pegs = 4
           colors = 6
           game = generateGame rows pegs colors gen
           wBoardLines = 3 + wBoardY (boardToWBoard $ BoardPosition rows 0)
           wBoardColumns = 3 + length "whites" + wBoardX (whitesPosition game 0)
           wControlsColumns = 4 + max (length " Pegs: " + 2 * colorCount game - 2)
                                      (length "+ Move: arrows +")
       in do
  start
  keypad stdScr True
  echo False
  refresh

  styles <- convertStyles [Style c BlackB | c <- [BlackF, RedF, GreenF, BlueF, YellowF, MagentaF, CyanF]]
  wBoard <- newWin wBoardLines wBoardColumns 0 0
  wControls <- newWin 6 wControlsColumns 2 (wBoardColumns + 5)
  wOutcome <- newWin 2 (length "press any key to exit  ") 9 (wBoardColumns + 5)


  let ui = UI {
             uiGame      = game
           , uiWOutcome  = wOutcome
           , uiWBoard    = wBoard
           , uiStyles    = styles
           } in do
      wBorder wBoard defaultBorder
      mvWAddStr wBoard 0 ((wBoardColumns - length " Board ") `div` 2) " Board "
      drawBoard ui

      wBorder wControls defaultBorder
      mvWAddStr wControls 0 ((wControlsColumns - length " Controls ") `div` 2) " Controls "
      mvWAddStr wControls 1 2 "Pegs: "
      mapM_ ((drawCodePeg ui wControls . toEnum) >=> (\_ -> waddch wControls $ toEnum $ fromEnum ' ')) [1 .. colorCount game]
      mvWAddStr wControls 2 2 "Move: arrows"
      mvWAddStr wControls 3 2 "Guess: g"
      mvWAddStr wControls 4 2 "Quit: q"
      wRefresh wControls

      let (WBoardPosition y x) = blacksPosition game 0 in
        mvWAddStr (uiWBoard ui) y x "Blacks"

      let (WBoardPosition y x) = whitesPosition game 0 in
        mvWAddStr (uiWBoard ui) y x "Whites"

      moveBoard ui $ modelToBoard ui (ModelPosition 0 0)
      handleInput ui

      echo True
      end

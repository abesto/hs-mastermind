module Main where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Model
import System.Random

data BoardPosition = BoardPosition Int Int
data ScreenPosition = ScreenPosition Int Int

toBoard :: ScreenPosition -> BoardPosition
toBoard (ScreenPosition y x) = BoardPosition
                                       ((y + 1) `div` 2 - 1)
                                       ((x + 1) `div` 2 - 1)

toScreen :: BoardPosition -> ScreenPosition
toScreen (BoardPosition y x) = ScreenPosition
                                       (y * 2 + 1)
                                       (x * 2 + 1)

castEnum :: Char -> ChType
castEnum = toEnum . fromEnum

putCh :: Char -> ScreenPosition -> IO ()
putCh c (ScreenPosition y x) = mvAddCh y x $ castEnum c

drawBox :: (Int, Int) -> (Int, Int) -> IO ()
drawBox (y0, x0) (y1, x1) = do
  mapM_ (putCh '|') [ScreenPosition y x | y <- [y0+1 .. y1-1], x <- [x0, x1]]
  mapM_ (putCh '-') [ScreenPosition y x | y <- [y0, y1], x <- [x0+1 .. x1]]
  mapM_ (putCh '+') [ScreenPosition y x | y <- [y0, y1], x <- [x0, x1]]

game :: Game
game = generateGame 10 4 6 (mkStdGen 3230)


drawCodePegs :: [CursesStyle] -> Game -> IO ()
drawCodePegs styles game = do
  mapM_ (f game) $ zip [0..] (codePegs $ code game)
  return ()
    where f game (n, p) = do
            setCursorField $ BoardPosition 0 n
            putCodePeg styles p

putCodePeg :: [CursesStyle] -> CodePeg -> IO ()
putCodePeg styles p = do
  setStyle $ styles !! fromEnum p
  wAddStr stdScr "*"
  resetStyle

drawBoard :: [CursesStyle] -> Game -> IO ()
drawBoard styles game = do
  erase
  mapM_ (uncurry drawBox) [
                         ((2*row, 2*column), (2*(row+1), 2*(column+1))) |
                         row <- [0 .. rowCount game - 1], column <- [0 .. pegCount game - 1]
                        ]
  drawCodePegs styles game

getCursorField :: IO BoardPosition
getCursorField = do
    (y, x) <- getYX stdScr
    return $ toBoard $ ScreenPosition y x

setCursorField :: BoardPosition -> IO ()
setCursorField p = move sy sx
    where (ScreenPosition sy sx) = toScreen p

debug :: Show a => a -> IO ()
debug game = do
  (y, x) <- getYX stdScr
  move 40 0
  wAddStr stdScr $ show game
  move y x

handleInput :: [CursesStyle] -> Game -> IO ()
handleInput styles game = do
  debug game
  refresh
  c <- getCh
  (BoardPosition y x) <- getCursorField
  let (_, sizeX) = (rowCount game, pegCount game) in
    case c of
      KeyLeft  -> if x > 0 then do
                      setCursorField $ BoardPosition y (x-1)
                      handleInput styles game
                  else
                      handleInput styles game
      KeyRight -> if x < (sizeX-1) then do
                      setCursorField $ BoardPosition y (x+1)
                      handleInput styles game
                  else
                      handleInput styles game
      KeyChar 'g' ->
          let game' = guess game in
          do
            setCursorField $ BoardPosition (rowCount game' - length (guesses game')) x
            handleInput styles game'
      KeyChar 'q' ->
          return ()
      KeyChar c ->
           let
               f [(i, "")] =
                   if i > 0 && i <= colorCount game then
                       do
                         (BoardPosition row column) <- getCursorField
                         putCodePeg styles $ toEnum i
                         setCursorField $ BoardPosition row column
                         handleInput styles $ modifyGuess game column (toEnum i)
                   else
                         handleInput styles game
               f _ = handleInput styles game
           in f $ reads [c]
      _ -> handleInput styles game


main :: IO ()
main = do
  start
  keypad stdScr True
  echo False

  styles <- convertStyles [Style c BlackB | c <- [BlackF, RedF, GreenF, BlueF, YellowF, MagentaF, CyanF]]

  drawBoard styles game
  setCursorField $ BoardPosition (rowCount game - 1) 0
  refresh
  handleInput styles game

  echo True
  end

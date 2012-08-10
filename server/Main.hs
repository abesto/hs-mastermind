module Main where

import Control.Monad

import Network
import System.IO
import System.Random
import Text.JSON (encode, decode, Result(Ok, Error), JSON)

import Model
import Serialization ()

main :: IO ()
main = withSocketsDo $ do
         socket <- listenOn $ UnixSocket "mastermind.sock"
         (h, _, _) <- accept socket
         hSetBuffering h LineBuffering

         gen <- getStdGen
         let game = generateGame 10 4 6 gen in do
           write h game
           handle h game
         sClose socket

write :: JSON a => Handle -> a -> IO ()
write h g = hPutStrLn h (encode g)

handle :: Handle -> Game -> IO ()
handle h game = do
  r <- decode `liftM` hGetLine h
  case r of
    (Error e) -> do
                  putStrLn e
                  write h game
                  handle h game
    (Ok g) -> let game' = guess $ setCurrentGuess game g in
              do
                write h game'
                when (outcome game' == Playing) $ handle h game'

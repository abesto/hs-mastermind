hs-mastermind
=============

ncurses-based mastermind in haskell

Uses functionality from the hscurses branch at https://github.com/abesto/hscurses. After I get the go-ahead for some of the stuff in it I found online, I'll submit a pull request and it will hopefully be available via Cabal. Until then, you have to install it manually; sorry.

run with

    cabal build && ./dist/build/main/main

The game is in a playable state. You'll get a new random game each time you run the application. Move around with the arrow keys; change guess peg with number keys. Guess by pressing 'g', quit with 'q'.

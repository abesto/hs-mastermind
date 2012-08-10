hs-mastermind
=============

ncurses-based mastermind in haskell

Uses functionality from the hscurses branch at https://github.com/abesto/hscurses. After I get the go-ahead for some of the stuff in it I found online, I'll submit a pull request and it will hopefully be available via Cabal. Until then, you have to install it manually; sorry.

run with

    cabal build && ./dist/build/main/main

The game is in a playable state. You'll get a new random game each time you run the application. Move around with the arrow keys; change guess peg with number keys. Guess by pressing 'g', quit with 'q'.

A server exposing the game generation and guess evaluation logic is also included. It currently listens on a unix socket. I plan to migrate the standalone version to use it, and also to create one or more solvers using it. An example session:

````
nc -U mastermind.sock

{"rowCount":10,"pegCount":4,"colorCount":6,"guesses":[[0,0,0,0]],"results":[],"outcome":0}
[1,1,2,2]
{"rowCount":10,"pegCount":4,"colorCount":6,"guesses":[[1,1,2,2],[0,0,0,0]],"results":[[1]],"outcome":0}
[3,3,4,4]
{"rowCount":10,"pegCount":4,"colorCount":6,"guesses":[[1,1,2,2],[3,3,4,4],[0,0,0,0]],"results":[[1],[1,0]],"outcome":0}
invalid json is ignored
{"rowCount":10,"pegCount":4,"colorCount":6,"guesses":[[1,1,2,2],[3,3,4,4],[0,0,0,0]],"results":[[1],[1,0]],"outcome":0}
["as", 3, "is", "anything else the server doesn't expect"]
{"rowCount":10,"pegCount":4,"colorCount":6,"guesses":[[1,1,2,2],[3,3,4,4],[0,0,0,0]],"results":[[1],[1,0]],"outcome":0}
But errors are logged to stdout, see below...
{"rowCount":10,"pegCount":4,"colorCount":6,"guesses":[[1,1,2,2],[3,3,4,4],[0,0,0,0]],"results":[[1],[1,0]],"outcome":0}
^D
````

Server output:

````
Malformed JSON: invalid token in this context invalid
Unable to read Int
Malformed JSON: invalid token in this context But erro
server: <socket: 4>: hGetLine: end of file
````

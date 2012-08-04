module Model where
    import Data.List
    import System.Random

    data CodePeg = Empty | Red | Green | Blue | Yellow | Magenta | Cyan
                 deriving (Eq, Show, Enum)
    data KeyPeg = White | Black
                deriving (Eq, Show)

    newtype Code = Code {codePegs :: [CodePeg]} deriving Show
    newtype Guess = Guess {guessPegs :: [CodePeg]} deriving Show
    newtype Result = Result {keyPegs :: [KeyPeg]} deriving Show

    data Outcome = Playing | Won | Lost deriving (Eq, Show)
    data Game = Game {
          code       :: Code
        , rowCount   :: Int
        , pegCount   :: Int
        , colorCount :: Int
        , guesses    :: [Guess]
        , results    :: [Result]
        , outcome    :: Outcome
        } deriving Show


    createGame :: Int -> Int -> Int -> Code -> Game
    createGame rows pegs colors code' = Game
             {
               code = code'
             , rowCount = rows
             , pegCount = pegs
             , colorCount = colors
             , guesses = [Guess $ replicate pegs Empty]
             , results = []
             , outcome = Playing
             }


    generateGame :: RandomGen g => Int -> Int -> Int -> g -> Game
    generateGame rows pegs colors gen =
        createGame rows pegs colors $ Code $ take pegs $
                   map toEnum $ randomRs (1, colors) gen


    evaluate :: Code -> Guess -> Result
    evaluate (Code c) (Guess g) = Result $
                                         blacks (indexedCode, indexedGuess) ++
                                         whites (indexedCode \\ indexedGuess) (indexedGuess \\ indexedCode)
      where
        indexedCode = zip [1..] c
        indexedGuess = zip [1..] g
        blacks = flip replicate Black . length . uncurry intersect
        whites [] _ = []
        whites _ [] = []
        whites cs ((gi, gp):gs) = case findIndex (\(i, p) -> i /= gi && p == gp) cs of
                                    (Just i) -> White : whites (dropNth i cs) gs
                                    _        -> whites cs gs
        dropNth n xs = take n xs ++ drop (n+1) xs


    modifyGuess :: Game -> Int -> CodePeg -> Game
    modifyGuess game i p =
        let (Guess g) = last $ guesses game in
        game { guesses = init (guesses game) ++ [Guess $ take i g ++ [p] ++ drop (i+1) g] }


    guess :: Game -> Game
    guess game
        | outcome game /= Playing = game
        | Empty `elem` guessPegs (last $ guesses game) = game
        | otherwise
            = game {
                guesses = guesses game ++ [Guess $ replicate (pegCount game) Empty]
              , results = results game ++ [result]
              , outcome = newOutcome
              } where
        result = evaluate (code game) (last $ guesses game)
        newOutcome
            | keyPegs result == replicate (pegCount game) Black =  Won
            | rowCount game < length (guesses game)             =  Lost
            | otherwise                                         =  Playing

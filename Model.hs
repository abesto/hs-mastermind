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
    createGame rowCount pegCount colorCount code = Game
             {
               code = code
             , rowCount = rowCount
             , pegCount = pegCount
             , colorCount = colorCount
             , guesses = [Guess $ take pegCount $ repeat Empty]
             , results = []
             , outcome = Playing
             }


    generateGame :: RandomGen g => Int -> Int -> Int -> g -> Game
    generateGame rowCount pegCount colorCount gen =
        createGame rowCount pegCount colorCount $ Code $ take pegCount $
                   map toEnum $ randomRs (1, colorCount) gen


    evaluate :: Code -> Guess -> Result
    evaluate (Code cs) (Guess gs) = Result $
                                    blacks (zip [1..] cs) (zip [1..] gs) ++
                                    whites (zip [1..] cs) (zip [1..] $ filter (not . (`elem` cs)) gs)
      where blacks _ [] = []
            blacks (c:cs) (g:gs)
                | c == g    = Black : blacks cs gs
                | otherwise = blacks cs gs
            whites _ [] = []
            whites cs (g@(gi, gp):gs) = f $ findIndex (\(i, p) -> i /= gi && p == gp) cs
                    where f (Just i) = White : whites (dropNth i cs) gs
                          f _        = whites cs gs
                          dropNth n xs = take n xs ++ drop (n+1) xs


    modifyGuess :: Game -> Int -> CodePeg -> Game
    modifyGuess game i p =
        let (Guess g) = last $ guesses game in
        game { guesses = take (length (guesses game) - 1) (guesses game) ++ [Guess ((take i g) ++ [p] ++ (drop (i+1) g))] }

    guess :: Game -> Game
    guess game
        | outcome game /= Playing
            = game
        | Empty `elem` (guessPegs $ last $ guesses game)
            = game
        | otherwise
            = game {
                guesses = guesses game ++ [Guess $ take (pegCount game) $ repeat Empty]
              , results = results game ++ [result]
              , outcome = newOutcome
              } where
        result = evaluate (code game) (last $ guesses game)
        newOutcome
            | length (keyPegs result) == pegCount game
              && all (\x -> x == Black) (keyPegs result)    = Won
            | rowCount game == (length (guesses game) + 1)  = Lost
            | otherwise                                     = Playing

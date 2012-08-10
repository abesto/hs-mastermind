module Serialization where
-- Based heavily on http://therning.org/magnus/archives/719

import Text.JSON.Generic
import Model
import Control.Monad
import Control.Applicative

mLookup :: Monad m => String -> [(String, a)] -> m a
mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

instance JSON Game where
    showJSON g = makeObj
                 [ ("rowCount", showJSON $ rowCount g)
                 , ("pegCount", showJSON $ pegCount g)
                 , ("colorCount", showJSON $ colorCount g)
                 , ("guesses", showJSON $ guesses g)
                 , ("results", showJSON $ results g)
                 , ("outcome", showJSON $ outcome g)
                 ]
    readJSON (JSObject obj) = let
                       get s = mLookup s (fromJSObject obj) >>= readJSON
                  in Game
                 <$> get "code"
                 <*> get "code"
                 <*> get "rowCount"
                 <*> get "pegCount"
                 <*> get "colorCount"
                 <*> get "guesses"
                 <*> get "results"
    readJSON _ = fail "Object expected for Game"


instance JSON Code where
    showJSON = showJSON . map showJSON . codePegs
    readJSON c = Code `liftM` readJSON c

instance JSON Guess where
    showJSON = showJSON . map showJSON . guessPegs
    readJSON g = Guess `liftM` readJSON g

instance JSON Model.Result where
    showJSON = showJSON . map showJSON . keyPegs
    readJSON r = Result `liftM` readJSON r

instance JSON Outcome where
    showJSON = showJSON . fromEnum
    readJSON o = toEnum `liftM` readJSON o

instance JSON CodePeg where
    showJSON = showJSON . fromEnum
    readJSON p = toEnum `liftM` readJSON p

instance JSON KeyPeg where
    showJSON = showJSON . fromEnum
    readJSON p = toEnum `liftM` readJSON p

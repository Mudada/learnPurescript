module State where

import Prelude
import Control.Monad.State
import Control.Monad.State.Class
import Data.Foldable (traverse_)
import Data.Unit
import Data.String (toCharArray)

countParens :: Char -> Int
countParens '(' = 1
countParens ')' = -1
countParens _ = 0

sumParens :: String -> State Int Unit
sumParens str = traverse_ checkParens charArray
    where charArray = toCharArray str
          checkParens = \c -> modify \sum -> (countParens c) + sum


module Reader where

import Prelude
import Control.Monad.Reader
import Data.Array (concat, replicate)
import Data.Foldable (foldl)
import Data.Monoid (power)
import FFI.Util (typeof)

type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line str = do
    indent <- ask
    pure $ flip (<>) str $ power "  " indent

indent :: Doc -> Doc
indent = local (\n -> n + 1)

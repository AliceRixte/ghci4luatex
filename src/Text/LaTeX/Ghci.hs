module Text.LaTeX.Ghci where

import Text.LaTeX
import Data.Text

printTex :: LaTeX -> IO ()
printTex  =
  putStrLn . unpack . render
module Lib
    ( someFunc
    ) where

import Veri.Parser
import Text.Parsec
import Veri.Alpha

someFunc :: IO ()
someFunc = do
    f <- lines <$> readFile "test.txt"
    case sequenceA [(parse  parseEpsilon "stdin" (head f)), (parse parseEpsilon "stdin" (head $ tail f))] of
        Left err -> print err
        Right [x, x1] -> print $ alphaEquivalent x x1
        _ -> print "ERR"
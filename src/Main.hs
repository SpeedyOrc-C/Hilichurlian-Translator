module Main where

import Hilichurlian.Parser (buildSyntaxTrees)
import Hilichurlian.Translator.ZhCn (toZhCn)
import Data.List (intercalate)
import Control.Arrow ( (>>>) )

translateZhCn input = toZhCn <$> buildSyntaxTrees input

prettyTranslateZhCn =
        translateZhCn
    >>> concat
    >>> intercalate "\n"
    >>> putStrLn

main = return ()

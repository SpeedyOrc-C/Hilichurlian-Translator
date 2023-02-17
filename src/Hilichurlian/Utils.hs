module Hilichurlian.Utils where

import Hilichurlian.Structure ( Phrase(..) )

import Control.Arrow ( (>>>) )
import Data.Char (toLower)
import Data.Tuple (swap)

-- Clean up a sentence before parsing it.
cleanUp :: String -> Phrase
cleanUp =
        filter (`notElem` ",.?!;:")
    >>> map toLower
    >>> words
    >>> Phrase

{-
Sentence Splitters
-}

-- Split a sentence into 2.
split2 :: [a] -> [([a], [a])]
split2 l = do
    i <- [1..length l - 1]
    [splitAt i l]

-- Allow empty
split2e :: [a] -> [([a], [a])]
split2e l = do
    i <- [0..length l]
    [splitAt i l]

-- Mirror
split2m :: [a] -> [([a], [a])]
split2m l = result ++ (swap <$> result)
    where result = split2 l

-- Mirror and allow empty
split2me :: [a] -> [([a], [a])]
split2me l = result ++ (swap <$> result)
    where result = split2e l

-- Split a sentence into 3.
split3 :: [a] -> [([a], [a], [a])]
split3 l = do
    i <- [1..length l - 2]
    (l1, l2) <- split2 (drop i l)
    [(take i l, l1, l2)]

split3e :: [a] -> [([a], [a], [a])]
split3e l = do
    i <- [0..length l]
    (l1, l2) <- split2e (drop i l)
    [(take i l, l1, l2)]

split3m :: [a] -> [([a], [a], [a])]
split3m l = result ++ ((\(a, b, c) -> (c, b, a)) <$> result)
    where result = split3 l

split3me :: [a] -> [([a], [a], [a])]
split3me l = result ++ ((\(a, b, c) -> (c, b, a)) <$> result)
    where result = split3e l
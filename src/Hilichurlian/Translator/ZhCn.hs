module Hilichurlian.Translator.ZhCn (toZhCn) where

import Hilichurlian.Structure
    ( DeterminatePhrase(..),
      Noun(..),
      NounPhrase(..),
      Phrase(Phrase),
      Sentence(VerbSentence),
      Verb(..),
      VerbPhrase(VerbTransitivePhrase) )
import Hilichurlian.Translator.LexicalDictionary
    ( LexicalDictionary )

import Data.Maybe (fromMaybe)


nounDictionary :: LexicalDictionary
nounDictionary = [
        ("mi", ["我"]),
        ("ye", ["你"])
    ]

verbDictionary :: LexicalDictionary
verbDictionary = [
        ("muhe", ["喜欢", "爱"])
    ]

class ToChineseSimplified a where
    toZhCn :: a -> [String]

instance ToChineseSimplified Sentence where
    toZhCn (VerbSentence determinatePhrase verbPhrase) = do
        dp <- toZhCn determinatePhrase
        vp <- toZhCn verbPhrase
        return $ dp ++ vp

instance ToChineseSimplified DeterminatePhrase where
    toZhCn (DeterminatePhrase nounPhrase Nothing) =
        toZhCn nounPhrase
    -- translateZHCN (DeterminatePhrase nounPhrase (Just determiner)) = do
    --     np <- translateZHCN nounPhrase
    --     d <- translateZHCN determiner
    --     return $ d ++ np

instance ToChineseSimplified NounPhrase where
    toZhCn (NounPhrase noun _ _) =
        toZhCn noun

instance ToChineseSimplified Noun where
    toZhCn (Noun (Phrase noun)) =
        fromMaybe [] (lookup (unwords noun) nounDictionary)

instance ToChineseSimplified Verb where
    toZhCn (Verb (Phrase verb)) =
        fromMaybe [] (lookup (unwords verb) verbDictionary)

instance ToChineseSimplified VerbPhrase where
    toZhCn (VerbTransitivePhrase verb nounPhrase _) = do
        v <- toZhCn verb
        np <- toZhCn nounPhrase
        return $ v ++ np

{-
Sentences for testing
-}

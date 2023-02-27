module Hilichurlian.Translator.ZhCn (toZhCn) where

import Hilichurlian.Structure
    ( DeterminatePhrase(..),
      Noun(..),
      NounPhrase(..),
      Phrase(..),
      Sentence(..),
      Verb(..),
      VerbPhrase(..), Adverb (..), AdjectivePhrase (..), Adjective (..), Determiner (..), PredicativePhrase (..) )
import Hilichurlian.Translator.LexicalDictionary

import Data.Maybe (fromMaybe)


nounDictionary :: LexicalDictionary
nounDictionary = [
        ("gusha", ["草", "水果", "谷物"]),
        ("mi", ["我"]),
        ("ye", ["你"]),
        ("yo", ["你"])
    ]

verbDictionary :: LexicalDictionary
verbDictionary = [
        ("mani", ["给"]),
        ("mito", ["知道"]),
        ("muhe", ["喜欢", "爱"])
    ]

adjectiveDictionary :: LexicalDictionary
adjectiveDictionary = [
        ("celi", ["热"]),
        ("celi boya", ["红色"]),
        ("dada", ["好"])
    ]

adjectiveSuffix :: String
adjectiveSuffix = "的"

adjectivePredicatePrefix :: String
adjectivePredicatePrefix = "很"

adverbDictionary :: LexicalDictionary
adverbDictionary = [
        ("dada", ["很"]),
        ("nye", ["不"])
    ]

determinatePredicatePrefix :: String
determinatePredicatePrefix = "是"

determinerDictionary :: LexicalDictionary
determinerDictionary = [
        ("dala", ["哪个"]),
        ("zido", ["这个"])
    ]


class ToChineseSimplified a where
    toZhCn :: a -> [String]

lookUpMultiple phrase dictionary =
    fromMaybe [] (lookup (unwords phrase) dictionary)

instance ToChineseSimplified Noun where
    toZhCn (Noun (Phrase n)) = lookUpMultiple n nounDictionary

instance ToChineseSimplified Verb where
    toZhCn (Verb (Phrase v)) = lookUpMultiple v verbDictionary

instance ToChineseSimplified Adverb where
    toZhCn (Adverb (Phrase adv)) = lookUpMultiple adv adverbDictionary

instance ToChineseSimplified Adjective where
    toZhCn (Adjective (Phrase a)) = lookUpMultiple a adjectiveDictionary

instance ToChineseSimplified Determiner where
    toZhCn (Determiner (Phrase d)) = lookUpMultiple d determinerDictionary

instance ToChineseSimplified Sentence where
    toZhCn (VerbSentence determinatePhrase verbPhrase) = do
        dp <- toZhCn determinatePhrase
        vp <- toZhCn verbPhrase
        return $ dp ++ vp

    toZhCn (PredicativeSentence determinatePhrase predicativePhrase) = do
        dp <- toZhCn determinatePhrase
        pp <- toZhCn predicativePhrase
        return $ dp ++ pp

instance ToChineseSimplified DeterminatePhrase where
    toZhCn (DeterminatePhrase nounPhrase (Just determiner)) = do
        np <- toZhCn nounPhrase
        d <- toZhCn determiner
        return $ d ++ np

    toZhCn (DeterminatePhrase nounPhrase Nothing) =
        toZhCn nounPhrase

instance ToChineseSimplified PredicativePhrase where
    toZhCn (DeterminatePredicate determinatePhrase) = do
        dp <- toZhCn determinatePhrase
        return $ determinatePredicatePrefix ++ dp

    toZhCn (AdjectivePredicate adjectivePhrase) = do
        ap <- toZhCn adjectivePhrase
        return $ adjectivePredicatePrefix ++ ap

instance ToChineseSimplified NounPhrase where
    toZhCn (NounPhrase noun (Just adjectivePhrase) _) = do
        n <- toZhCn noun
        ap <- toZhCn adjectivePhrase
        return $ ap ++ n

    toZhCn (NounPhrase noun Nothing _) =
        toZhCn noun

instance ToChineseSimplified AdjectivePhrase where
    toZhCn (AdjectivePhrase adjective (Just adverb)) = do
        a <- toZhCn adjective
        adv <- toZhCn adverb
        return $ adv ++ a

    toZhCn (AdjectivePhrase adjective Nothing) =
        toZhCn adjective

instance ToChineseSimplified VerbPhrase where
    toZhCn (VerbIntransitivePhrase verb (Just adverb)) = do
        v <- toZhCn verb
        adv <- toZhCn adverb
        return $ adv ++ v

    toZhCn (VerbIntransitivePhrase verb Nothing) =
        toZhCn verb

    toZhCn (VerbTransitivePhrase verb nounPhrase (Just adverb)) = do
        v <- toZhCn verb
        np <- toZhCn nounPhrase
        adv <- toZhCn adverb
        return $ adv ++ v ++ np

    toZhCn (VerbTransitivePhrase verb nounPhrase Nothing) = do
        v <- toZhCn verb
        np <- toZhCn nounPhrase
        return $ v ++ np

    toZhCn (VerbTransitive2ObjPhrase verb nounPhrase1 nounPhrase2 (Just adverb)) = do
        v <- toZhCn verb
        np1 <- toZhCn nounPhrase1
        np2 <- toZhCn nounPhrase2
        adv <- toZhCn adverb
        return $ adv ++ v ++ np1 ++ np2
    
    toZhCn (VerbTransitive2ObjPhrase verb nounPhrase1 nounPhrase2 Nothing) = do
        v <- toZhCn verb
        np1 <- toZhCn nounPhrase1
        np2 <- toZhCn nounPhrase2
        return $ v ++ np1 ++ np2

    toZhCn (VerbRecursivePhrase verb sentence (Just adverb)) = do
        v <- toZhCn verb
        adv <- toZhCn adverb
        s <- toZhCn sentence
        return $ adv ++ v ++ s

    toZhCn (VerbRecursivePhrase verb sentence Nothing) = do
        v <- toZhCn verb
        s <- toZhCn sentence
        return $ v ++ s

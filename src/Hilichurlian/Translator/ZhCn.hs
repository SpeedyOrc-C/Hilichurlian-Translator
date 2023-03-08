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
        ("biaodomu", ["坏陌生人"]),
        ("boya", ["颜色"]),
        ("celi boya", ["红色"]),
        ("unu boya", ["黄色"]),
        ("lata boya", ["蓝色"]),
        ("nini boya", ["白色"]),
        ("nunu boya", ["黑色"]),
        ("sama boya", ["黑色"]),
        ("buka", ["肚子"]),
        ("celi", ["火"]),
        ("celi lata", ["萤火虫", "星星"]),
        ("celi lata gusha", ["小灯草"]),
        ("celi upa", ["太阳"]),
        ("dadaupa", ["达达乌帕谷"]),
        ("dada upa", ["高山"]),
        ("dala", ["什么"]),
        ("domu", ["家人"]),
        ("eleka", ["部落"]),
        ("gusha", ["草", "水果", "谷物"]),
        ("homu", ["吼姆"]),
        ("in movo lata", ["鱼", "青蛙"]),
        ("kucha gusha", ["种子"]),
        ("kuzi", ["野猪"]),
        ("lata", ["冰"]),
        ("lata movo", ["水"]),
        ("lawa", ["首领"]),
        ("mani", ["手"]),
        ("mi", ["我"]),
        ("mimi", ["我们"]),
        ("mita", ["肉"]),
        ("mita in movo lata", ["鱼", "青蛙"]),
        ("mitono", ["丘丘人"]),
        ("mosi", ["食物"]),
        ("mosino", ["食物"]),
        ("movo lata", ["水"]),
        ("nini", ["空气"]),
        ("odomu", ["陌生人"]),
        ("sada", ["石头"]),
        ("tiga", ["木棍"]),
        ("tomo", ["朋友"]),
        ("upa", ["天"]),
        ("upakundala", ["闪电"]),
        ("upano", ["鸟"]),
        ("ya", ["人"]),
        ("yaya", ["人们"]),
        ("ye", ["你"]),
        ("yo", ["你"])
    ]

verbDictionary :: LexicalDictionary
verbDictionary = [
        ("beru", ["说"]),
        ("biat", ["肏"]),
        ("celi", ["烧"]),
        ("guru", ["饿"]),
        ("kundala", ["打败"]),
        ("mani", ["给"]),
        ("mito", ["知道"]),
        ("mosi", ["吃"]),
        ("movo", ["移动"]),
        ("muhe", ["喜欢", "爱"]),
        ("nini", ["消失"]),
        ("nunu", ["睡觉"]),
        ("plata", ["打"]),
        ("plama", ["打"]),
        ("shato", ["挡住"]),
        ("todo", ["给"])
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

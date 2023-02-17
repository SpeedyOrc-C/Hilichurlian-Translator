module Hilichurlian.Parser (buildSyntaxTrees) where

import Hilichurlian.Structure
    ( Adjective(..),
      AdjectivePhrase(..),
      Adverb(..),
      DeterminatePhrase(..),
      Determiner(..),
      Noun(..),
      NounPhrase(..),
      Phrase(..),
      PredicativePhrase(..),
      Preposition(..),
      PrepositionalPhrase(..),
      Sentence(..),
      Verb(..),
      VerbPhrase(..) )
import Hilichurlian.Words
    ( allAdjectives,
      allAdverbs,
      allDeterminers,
      allIntransitiveVerbs,
      allNouns,
      allObject,
      allPrepositions,
      allRecursiveVerbs,
      allSubjectIntransitive,
      allSubjectTransitive,
      allTransitive2ObjVerbs,
      allTransitiveVerbs,
      allVerbs )
import Hilichurlian.Utils ( cleanUp, split2, split2e, split2me )

import Data.List (nub, intercalate)
import Control.Arrow ((>>>))

buildSyntaxTrees :: String -> [Sentence]
buildSyntaxTrees = cleanUp >>> parseSentence

{-
Check whether a phrase is in a specified category.
-}

isNoun :: [String] -> Bool
isNoun = (`elem` allNouns)

isVerb :: [String] -> Bool
isVerb = (`elem` allVerbs)

isIntransitiveVerb :: [String] -> Bool
isIntransitiveVerb = (`elem` allIntransitiveVerbs)

isTransitiveVerb :: [String] -> Bool
isTransitiveVerb = (`elem` allTransitiveVerbs)

isTransitive2ObjVerb :: [String] -> Bool
isTransitive2ObjVerb = (`elem` allTransitive2ObjVerbs)

isRecursiveVerb :: [String] -> Bool
isRecursiveVerb = (`elem` allRecursiveVerbs)

isAdjective :: [String] -> Bool
isAdjective = (`elem` allAdjectives)

isAdverb :: [String] -> Bool
isAdverb = (`elem` allAdverbs)

isPreposition :: [String] -> Bool
isPreposition = (`elem` allPrepositions)

isDeterminer :: [String] -> Bool
isDeterminer = (`elem` allDeterminers)

isObject :: [String] -> Bool
isObject = (`elem` allObject)

nounPhraseIsObject :: NounPhrase -> Bool
nounPhraseIsObject (NounPhrase (Noun (Phrase noun)) _ _) = isObject noun

isSubjectTransitive :: [String] -> Bool
isSubjectTransitive = (`elem` allSubjectTransitive)

isSubjectIntransitive :: [String] -> Bool
isSubjectIntransitive = (`elem` allSubjectIntransitive)

{-
Parsers
-}

type Parser wordCategory = Phrase -> [wordCategory]

-- Parsing leaves are easy.

parseNoun :: Parser Noun
parseNoun p@(Phrase ws) = [Noun p | isNoun ws]

parseVerb :: Parser Verb
parseVerb p@(Phrase ws) = [Verb p | isVerb ws]

parseIntransitiveVerb :: Parser Verb
parseIntransitiveVerb p@(Phrase ws) = [Verb p | isIntransitiveVerb ws]

parseTransitiveVerb :: Parser Verb
parseTransitiveVerb p@(Phrase ws) = [Verb p | isTransitiveVerb ws]

parseTransitive2ObjVerb :: Parser Verb
parseTransitive2ObjVerb p@(Phrase ws) = [Verb p | isTransitive2ObjVerb ws]

parseRecursiveVerb :: Parser Verb
parseRecursiveVerb p@(Phrase ws) = [Verb p | isRecursiveVerb ws]

parseAdjective :: Parser Adjective
parseAdjective p@(Phrase ws) = [Adjective p | isAdjective ws]

parseAdverb :: Parser Adverb
parseAdverb p@(Phrase ws) = [Adverb p | isAdverb ws]

parsePreposition :: Parser Preposition
parsePreposition p@(Phrase ws) = [Preposition p | isPreposition ws]

parseDeterminer :: Parser Determiner
parseDeterminer p@(Phrase ws) = [Determiner p | isDeterminer ws]

parseObject :: Parser Noun
parseObject p@(Phrase ws) = [Noun p | isObject ws]

parseSubjectTransitive :: Parser Noun
parseSubjectTransitive p@(Phrase ws) = [Noun p | isSubjectTransitive ws]

parseSubjectIntransitive :: Parser Noun
parseSubjectIntransitive p@(Phrase ws) = [Noun p | isSubjectIntransitive ws]

-- Parsing tree nodes is hard.

{-
lata (dada)
(very) cold

lata: cold
dada: very
-}
parseAdjectivePhrase :: Parser AdjectivePhrase
parseAdjectivePhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2me phrase
    adjective <- parseAdjective (Phrase left)
    if null right then
        return $ AdjectivePhrase
            adjective
            Nothing
    else do
        adverb <- parseAdverb (Phrase right)
        return $ AdjectivePhrase
            adjective
            (Just adverb)

{-
in movo lata
fish / (meat) in the water

in: in
movo: move
lata: ice
-}
parsePrepositionPhrase :: Parser PrepositionalPhrase
parsePrepositionPhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2 phrase
    preposition <- parsePreposition (Phrase left)
    nounPhrase <- parseNounPhrase (Phrase right)
    return $ PrepositionalPhrase
        preposition
        nounPhrase

{-
gusha gusha boya dada
very green grass

gusha: grass
gusha boya: green
dada: very
-}
parseNounPhrase :: Parser NounPhrase
parseNounPhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2me phrase
    noun <- parseNoun (Phrase left)
    ~(left', right') <- split2me right
    if null left' then
        if null right' then
            return $ NounPhrase
                noun
                Nothing
                Nothing
        else do
            prepositionalPhrase <- parsePrepositionPhrase (Phrase right')
            return $ NounPhrase
                noun
                Nothing
                (Just prepositionalPhrase)
    else
        if null right' then do
            adjectivePhrase <- parseAdjectivePhrase (Phrase left')
            return $ NounPhrase
                noun
                (Just adjectivePhrase)
                Nothing
        else do
            prepositionalPhrase <- parsePrepositionPhrase (Phrase right')
            adjectivePhrase <- parseAdjectivePhrase (Phrase left')
            return $ NounPhrase
                noun
                (Just adjectivePhrase)
                (Just prepositionalPhrase)

parseDeterminatePhrase :: Parser DeterminatePhrase
parseDeterminatePhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2e phrase
    nounPhrase <- parseNounPhrase (Phrase left)
    if null right then
        return $ DeterminatePhrase
            nounPhrase
            Nothing
    else do
        determiner <- parseDeterminer (Phrase right)
        return $ DeterminatePhrase
            nounPhrase
            (Just determiner)

parseVerbIntransitivePhrase :: Parser VerbPhrase
parseVerbIntransitivePhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2e phrase
    verb <- parseIntransitiveVerb (Phrase left)
    if null right then
        return $ VerbIntransitivePhrase
            verb
            Nothing
    else do
        adverb <- parseAdverb (Phrase right)
        return $ VerbIntransitivePhrase
            verb
            (Just adverb)

parseVerbTransitivePhrase :: Parser VerbPhrase
parseVerbTransitivePhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2 phrase
    verb <- parseTransitiveVerb (Phrase left)
    ~(left', right') <- split2e right
    nounPhrase <- filter nounPhraseIsObject $ parseNounPhrase (Phrase left')
    if null right' then
        return $ VerbTransitivePhrase
            verb
            nounPhrase
            Nothing
    else do
        adverb <- parseAdverb (Phrase right')
        return $ VerbTransitivePhrase
            verb
            nounPhrase
            (Just adverb)

parseVerbTransitive2ObjPhrase :: Parser VerbPhrase
parseVerbTransitive2ObjPhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2 phrase
    verb <- parseTransitive2ObjVerb (Phrase left)
    ~(left', right') <- split2 right
    nounPhrase <- filter nounPhraseIsObject $ parseNounPhrase (Phrase left')
    ~(left'', right'') <- split2e right'
    nounPhrase' <- filter nounPhraseIsObject $ parseNounPhrase (Phrase left'')
    if null right'' then
        return $ VerbTransitive2ObjPhrase
            verb
            nounPhrase
            nounPhrase'
            Nothing
    else do
        adverb <- parseAdverb (Phrase right'')
        return $ VerbTransitive2ObjPhrase
            verb
            nounPhrase
            nounPhrase'
            (Just adverb)

parseVerbRecursivePhrase :: Parser VerbPhrase
parseVerbRecursivePhrase (Phrase phrase) = nub $ do
    ~(left, right) <- split2 phrase
    ~(left', right') <- split2e left
    verb <- parseVerb (Phrase left')
    sentence <- parseSentence (Phrase right) -- undefined is "parseSentence"
    if null right' then
        return $ VerbRecursivePhrase
            verb
            sentence
            Nothing
    else do
        adverb <- parseAdverb (Phrase right')
        return $ VerbRecursivePhrase
            verb
            sentence
            (Just adverb)

parseDeterminatePredicate :: Parser PredicativePhrase
parseDeterminatePredicate phrase = do
    determinatePhrase <- parseDeterminatePhrase phrase
    return $ DeterminatePredicate determinatePhrase

parseAdjectivePredicate :: Parser PredicativePhrase
parseAdjectivePredicate phrase = do
    adjectivePhrase <- parseAdjectivePhrase phrase
    return $ AdjectivePredicate adjectivePhrase

parsePrepositionalPredicate :: Parser PredicativePhrase
parsePrepositionalPredicate phrase = do
    prepositionalPhrase <- parsePrepositionPhrase phrase
    return $ PrepositionalPredicate prepositionalPhrase

parsePredicativePhrase :: Parser PredicativePhrase
parsePredicativePhrase phrase = concat [
        parsePrepositionalPredicate phrase,
        parseDeterminatePredicate phrase,
        parseAdjectivePredicate phrase
    ]

parseVerbPhrase :: Parser VerbPhrase
parseVerbPhrase phrase = concat [
        parseVerbTransitive2ObjPhrase phrase,
        parseVerbTransitivePhrase phrase,
        parseVerbIntransitivePhrase phrase,
        parseVerbRecursivePhrase phrase
    ]

parseVerbSentence :: Parser Sentence
parseVerbSentence (Phrase phrase) = nub $ do
    ~(left, right) <- split2 phrase
    determinatePhrase <- parseDeterminatePhrase (Phrase left)
    verbPhrase <- parseVerbPhrase (Phrase right)
    return $ VerbSentence
        determinatePhrase
        verbPhrase

parsePredicativeSentence :: Parser Sentence
parsePredicativeSentence (Phrase phrase) = nub $ do
    ~(left, right) <- split2 phrase
    determinatePhrase <- parseDeterminatePhrase (Phrase left)
    predicativePhrase <- parsePredicativePhrase (Phrase right)
    return $ PredicativeSentence
        determinatePhrase
        predicativePhrase

parseSentence :: Parser Sentence
parseSentence phrase = 
    parseVerbSentence phrase ++
    parsePredicativeSentence phrase

prettyLines :: Show a => [a] -> IO ()
prettyLines x = putStrLn $ intercalate "\n" (show <$> x)

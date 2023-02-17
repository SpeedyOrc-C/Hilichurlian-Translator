module Hilichurlian.Structure where

import Data.Maybe (isJust, fromJust)


newtype Phrase = Phrase [String]
    deriving (Eq)

instance Show Phrase where
    show (Phrase phrase) = unwords phrase


data Sentence
    = VerbSentence
        DeterminatePhrase
        VerbPhrase
    | PredicativeSentence
        DeterminatePhrase
        PredicativePhrase
    deriving (Eq)

instance Show Sentence where
    show (VerbSentence determinatePhrase verbPhrase) =
        "S(" ++
        show determinatePhrase ++ ", " ++
        show verbPhrase ++
        ")"

    show (PredicativeSentence determinatePhrase predicativePhrase) =
        "S(" ++
        show determinatePhrase ++ ", " ++
        show predicativePhrase ++
        ")"


data PredicativePhrase
    = DeterminatePredicate
        DeterminatePhrase
    | AdjectivePredicate
        AdjectivePhrase
    | PrepositionalPredicate
        PrepositionalPhrase
    deriving (Eq)

instance Show PredicativePhrase where
    show (DeterminatePredicate determinatePhrase) =
        "Pred(" ++ show determinatePhrase ++ ")"
    show (AdjectivePredicate adjectivePhrase) =
        "Pred(" ++ show adjectivePhrase ++ ")"
    show (PrepositionalPredicate prepositionalPhrase) =
        "Pred(" ++ show prepositionalPhrase ++ ")"


data NounPhrase
    = NounPhrase
        Noun
        (Maybe AdjectivePhrase)
        (Maybe PrepositionalPhrase)
    deriving (Eq)

instance Show NounPhrase where
    show (NounPhrase noun maybeAdjectivePhrase maybePrepositionalPhrase) =
        "NP(" ++
        show noun ++
        maybe "" (\x -> ", " ++ show x) maybeAdjectivePhrase ++
        maybe "" (\x -> ", " ++ show x) maybePrepositionalPhrase ++
        ")"


data AdjectivePhrase
    = AdjectivePhrase
        Adjective
        (Maybe Adverb)
    deriving (Eq)

instance Show AdjectivePhrase where
    show (AdjectivePhrase adjective maybeAdverb) =
        "AP(" ++
        show adjective ++
        maybe "" (\x -> ", " ++ show x) maybeAdverb ++
        ")"
        

data PrepositionalPhrase
    = PrepositionalPhrase
        Preposition
        NounPhrase
    deriving (Eq)

instance Show PrepositionalPhrase where
    show (PrepositionalPhrase preposition noun) =
        "PrepP(" ++
        show preposition ++ ", " ++
        show noun ++
        ")"


data DeterminatePhrase
    = DeterminatePhrase
        NounPhrase
        (Maybe Determiner)
    deriving (Eq)

instance Show DeterminatePhrase where
    show (DeterminatePhrase nounPhrase maybeDeterminer) =
        "DP(" ++
        show nounPhrase ++
        maybe "" (\x -> ", " ++ show x) maybeDeterminer ++
        ")"


data VerbPhrase
    = VerbIntransitivePhrase
        Verb
        (Maybe Adverb)
    | VerbTransitivePhrase
        Verb
        NounPhrase
        (Maybe Adverb)
    | VerbTransitive2ObjPhrase
        Verb
        NounPhrase
        NounPhrase
        (Maybe Adverb)
    | VerbRecursivePhrase
        Verb
        Sentence
        (Maybe Adverb)
    deriving (Eq)

instance Show VerbPhrase where
    show (VerbIntransitivePhrase verb maybeAdverb) =
        "VP(" ++
        show verb ++
        maybe "" (\x -> ", " ++ show x) maybeAdverb ++
        ")"

    show (VerbTransitivePhrase verb nounPhrase maybeAdverb) =
        "VP(" ++
        show verb ++ ", " ++
        show nounPhrase ++
        maybe "" (\x -> ", " ++ show x) maybeAdverb ++
        ")"

    show (VerbTransitive2ObjPhrase verb nounPhrase nounPhrase' maybeAdverb) =
        "VP(" ++
        show verb ++ ", " ++
        show nounPhrase ++ ", " ++
        show nounPhrase' ++
        maybe "" (\x -> ", " ++ show x) maybeAdverb ++
        ")"

    show (VerbRecursivePhrase verb sentence maybeAdverb) =
        "VP(" ++
        show verb ++ ", " ++
        show sentence ++
        maybe "" (\x -> ", " ++ show x) maybeAdverb ++
        ")"
    
{-
Leaves of the syntax tree.
Therefore, they can no longer be split.
-}

newtype Noun = Noun Phrase
    deriving (Eq)

instance Show Noun where
    show :: Noun -> String
    show (Noun noun) = "N(" ++ show noun ++ ")"


newtype Verb = Verb Phrase
    deriving (Eq)

instance Show Verb where
    show :: Verb -> String
    show (Verb verb) = "V(" ++ show verb ++ ")"


newtype Adjective = Adjective Phrase
    deriving (Eq)

instance Show Adjective where
    show :: Adjective -> String
    show (Adjective adjective) = "A(" ++ show adjective ++ ")"


newtype Adverb = Adverb Phrase
    deriving (Eq)

instance Show Adverb where
    show :: Adverb -> String
    show (Adverb adverb) = "Adv(" ++ show adverb ++ ")"


newtype Preposition = Preposition Phrase
    deriving (Eq)

instance Show Preposition where
    show :: Preposition -> String
    show (Preposition preposition) = "Prep(" ++ show preposition ++ ")" 


newtype Determiner = Determiner Phrase
    deriving (Eq)

instance Show Determiner where
    show :: Determiner -> String
    show (Determiner determiner) = "D(" ++ show determiner ++ ")"

module Hilichurlian.Translator.LexicalDictionary where

import Hilichurlian.Structure
    ( DeterminatePhrase(DeterminatePhrase),
      Noun(Noun),
      NounPhrase(NounPhrase),
      Phrase(Phrase),
      Sentence(VerbSentence),
      Verb(Verb),
      VerbPhrase(VerbTransitivePhrase) )


type LexicalDictionary = [(String, [String])]

iLoveYou =
    VerbSentence
        (DeterminatePhrase
            (NounPhrase
                (Noun (Phrase ["mi"]))
                Nothing
                Nothing
            )
            Nothing
        )
        (VerbTransitivePhrase
            (Verb (Phrase ["muhe"]))
            (NounPhrase
                (Noun (Phrase ["ye"]))
                Nothing
                Nothing
            )
            Nothing
        )

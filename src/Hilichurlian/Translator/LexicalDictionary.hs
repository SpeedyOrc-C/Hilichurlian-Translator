module Hilichurlian.Translator.LexicalDictionary where

import Hilichurlian.Structure
    ( DeterminatePhrase(DeterminatePhrase),
      Noun(Noun),
      NounPhrase(NounPhrase),
      Phrase(Phrase),
      Sentence(VerbSentence),
      Verb(Verb),
      VerbPhrase(VerbTransitivePhrase, VerbTransitive2ObjPhrase), AdjectivePhrase (AdjectivePhrase), Adjective (Adjective) )


type LexicalDictionary = [(String, [String])]

iLoveYou = -- Mi muhe ye.
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
            (DeterminatePhrase
                (NounPhrase
                    (Noun (Phrase ["ye"]))
                    Nothing
                    Nothing
                )
                Nothing
            )
            Nothing
        )

iGiveYouARedFruit = -- Mi mani ye gusha celi boya.
    VerbSentence
        (DeterminatePhrase
            (NounPhrase
                (Noun (Phrase ["mi"]))
                Nothing
                Nothing
            )
            Nothing
        )
        (VerbTransitive2ObjPhrase
            (Verb (Phrase ["mani"]))
            (DeterminatePhrase
                (NounPhrase
                    (Noun (Phrase ["ye"]))
                    Nothing
                    Nothing
                )
                Nothing
            )
            (DeterminatePhrase
                (NounPhrase
                    (Noun (Phrase ["gusha"]))
                    (Just (AdjectivePhrase
                        (Adjective (Phrase ["celi", "boya"]))
                        Nothing
                    ))
                    Nothing
                )
                Nothing
            )
            Nothing
        )

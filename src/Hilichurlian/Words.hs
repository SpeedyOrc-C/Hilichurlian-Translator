{-
Whether miHoYo or HoYoVerse please give us more Hilichurlian words.
-}

module Hilichurlian.Words where

import Data.List (nub)


allWords = [
        ["aba"],
        ["beru"],
        ["biadam"],
        ["biaodomu"],
        ["biat"],
        ["boya"],
        ["buka"],
        ["celi"],
        ["celi", "lata"],
        ["celi", "upa"],
        ["da"],
        ["dada"],
        ["dadaupa"],
        ["dala"],
        ["domu"],
        ["du"],
        ["dudu"],
        ["eleka"],
        ["guru"],
        ["gusha"],
        ["homu"],
        ["ika"],
        ["in"],
        ["ka"],
        ["kucha"],
        ["kucha", "gusha"],
        ["kundala"],
        ["kuzi"],
        ["lata"],
        ["lata", "movo"],
        ["lawa"],
        ["mani"],
        ["mi"],
        ["mimi"],
        ["mita"],
        ["mito"],
        ["mitono"],
        ["mosi"],
        ["mosi", "gusha"],
        ["mosi", "mita"],
        ["mosino"],
        ["movo"],
        ["muhe"],
        ["nesina"],
        ["nini"],
        ["nunu"],
        ["nya"],
        ["nye"],
        ["odomu"],
        ["olah"],
        ["oto"],
        ["plata"],
        ["plama"],
        ["pupu"],
        ["sada"],
        ["sama"],
        ["si"],
        ["shato"],
        ["tiga"],
        ["todo"],
        ["tomo"],
        ["ulena"],
        ["unta"],
        ["unu"],
        ["unu", "du"],
        ["unu", "gusha"],
        ["upa"],
        ["upakundala"],
        ["upano"],
        ["valo"],
        ["vin"],
        ["wei"],
        ["ya"],
        ["yaya"],
        ["ye"],
        ["yeye"],
        ["yo"],
        ["yoyo"],
        ["zido"]
    ]

allNouns = [
        ["biaodomu"],
        ["boya"],
        ["celi", "boya"],
        ["unu", "boya"],
        ["gusha", "boya"],
        ["lata", "boya"],
        ["nini", "boya"],
        ["nunu", "boya"],
        ["sama", "boya"],
        ["buka"],
        ["celi"],
        ["celi", "lata"],
        ["celi", "lata", "gusha"],
        ["celi", "upa"],
        ["dadaupa"],
        ["dada", "upa"],
        ["dala"],
        ["domu"],
        ["du"],
        ["dudu"],
        ["eleka"],
        ["gusha"],
        ["homu"],
        ["in", "movo", "lata"],
        ["kucha", "gusha"],
        ["kundala"],
        ["kuzi"],
        ["lata"],
        ["lata", "movo"],
        ["lawa"],
        ["mani"],
        ["mi"],
        ["mimi"],
        ["mita"],
        ["mita", "in", "movo", "lata"],
        ["mita", "movo", "lata"],
        ["mitono"],
        ["mosi"],
        ["mosino"],
        ["movo", "lata"],
        ["nini"],
        ["odomu"],
        ["pupu"],
        ["sada"],
        ["sama"],
        ["tiga"],
        ["tomo"],
        ["unu"],
        ["unu", "du"],
        ["unu", "gusha"],
        ["upa"],
        ["upakundala"],
        ["upano"],
        ["vin"],
        ["wei"],
        ["ya"],
        ["yaya"],
        ["ye"],
        ["yeye"],
        ["zido"]
    ]

allCommonPronouns = [
        ["mi"],
        ["mimi"],
        ["ya"],
        ["yaya"]
    ]

allErgativePronouns = [
        ["yo"],
        ["yo yo"]
    ]

allAbsolutivePronouns = [
        ["ye"],
        ["yeye"]
    ]

allObject = nub $
    allNouns ++
    allCommonPronouns ++
    allAbsolutivePronouns

allSubjectTransitive = nub $
    allNouns ++
    allCommonPronouns ++
    allErgativePronouns

allSubjectIntransitive = nub $
    allNouns ++
    allCommonPronouns ++
    allAbsolutivePronouns ++
    allErgativePronouns

allVerbs = [
        ["beru"],
        ["biat"],
        ["celi"],
        ["guru"],
        ["kundala"],
        ["mani"],
        ["mito"],
        ["mosi"],
        ["movo"],
        ["muhe"],
        ["nini"],
        ["nunu"],
        ["plata"],
        ["plama"],
        ["shato"],
        ["todo"]
    ]

allIntransitiveVerbs = [
        ["beru"],
        ["biat"],
        ["celi"],
        ["guru"],
        ["mani"],
        ["mito"],
        ["mosi"],
        ["movo"],
        ["muhe"],
        ["nini"],
        ["nunu"],
        ["plata"],
        ["plama"],
        ["shato"],
        ["todo"]
    ]

allTransitiveVerbs = [
        ["beru"],
        ["biat"],
        ["celi"],
        ["kundala"],
        ["mani"],
        ["mito"],
        ["mosi"],
        ["muhe"],
        ["plata"],
        ["plama"],
        ["shato"],
        ["todo"]
    ]

allTransitive2ObjVerbs = [
        ["mani"],
        ["todo"]
    ]

allRecursiveVerbs = [
        ["beru"],
        ["mito"],
        ["muhe"]
    ]

allAdjectives = [
        ["aba"],
        ["celi"],
        ["celi", "boya"],
        ["da"],
        ["dada"],
        ["du"],
        ["dudu"],
        ["gusha"],
        ["gusha", "boya"],
        ["ika"],
        ["kucha"],
        ["lata"],
        ["lata", "boya"],
        ["mita"],
        ["nini", "boya"],
        ["nunu", "boya"],
        ["oto"],
        ["sada"],
        ["sama", "boya"],
        ["unta"],
        ["unu"],
        ["unu", "boya"],
        ["unu", "du"],
        ["upa"]
    ]

allAdverbs = [
        ["aba"],
        ["da"],
        ["dada"],
        ["nye"],
        ["unta"]
    ]

allPrepositions = [
        ["in"]
    ]

allDeterminers = [
        ["zido"],
        ["dala"]
    ]

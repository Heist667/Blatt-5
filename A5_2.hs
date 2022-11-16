{-
Zum Vergleich des Argumentes s in isCold ist eine Ableitung der Klasse Eq notwendig.
Durch das Ableiten aus der Klasse Ord werden die Jahreszeiten in eine totale
Ordnung gebracht und die Operationen <, >, <=, >= für die Funktion beforeAutmn ermöglicht.
Durch das Ableiten aus der Klasse Enum werden die Jahreszeiten den Integern von 0-3 zugeordnet.
Das Ableiten der Klasse Show ermögicht die Ausgabe von Spring bzw Enum 0 in printFirstSeason 
in der Konsole.
-}


data Season = Spring | Summer | Autumn | Winter deriving (Eq, Ord, Enum, Show)

-- Testing if a season is cold
isCold :: Season -> Bool
isCold s
    | s == Winter = True
    | otherwise = False
-- Is there sill time until it is autumn?
beforeAutumn :: Season -> Bool
beforeAutumn s
    | s < Autumn = True
    | otherwise = False
-- Print first season
printFirstSeason :: Season
printFirstSeason = toEnum 0
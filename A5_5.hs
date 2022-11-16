{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
data Currency = MkCurrency {
    coins :: Integer,
    cents :: Integer,
    currency :: String
}

instance Show Currency where
    show(MkCurrency coins cents currency) = 
        if currency == "€" || currency == "$" 
        then show coins ++ "," ++ show cents ++ " " ++ currency
        else show coins ++ " " ++ currency

myEuro :: Currency
myEuro = MkCurrency 12 03 "€"

myDollar :: Currency
myDollar = MkCurrency 14 60 "$"

myYen :: Currency
myYen = MkCurrency 120 0 "Yen"
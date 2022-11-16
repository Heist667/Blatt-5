{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
data Currency = MkCurrency {
    coins :: Integer,
    cents :: Integer,
    currency :: String
}

getCoins :: Currency -> Integer
getCoins (MkCurrency coins cents currency) = coins

getCents :: Currency -> Integer
getCents (MkCurrency coins cents currency) = cents

getCurrency :: Currency -> String
getCurrency (MkCurrency coins cents currency) = currency

instance Show Currency where
    show(MkCurrency coins cents currency) = 
        if currency == "Euro" || currency == "Dollar" 
        then show coins ++ "," ++ show cents ++ " " ++ currency
        else show coins ++ " " ++ currency

myEuro :: Currency
myEuro = MkCurrency 12 03 "Euro"

myDollar :: Currency
myDollar = MkCurrency 14 60 "Dollar"

myYen :: Currency
myYen = MkCurrency 120 0 "Yen"

currencyToFloat :: Currency -> Float
currencyToFloat currency = fromIntegral (getCoins currency) + fromIntegral (getCents currency)/(10^2)

exchangeRate :: Currency -> Float -> Float
exchangeRate currency rate = if rate >= 1.0 then currencyToFloat currency/rate else currencyToFloat currency*rate

exchangeCurrency :: Currency -> Float -> String -> Currency
exchangeCurrency currency rate newcurrency = MkCurrency (truncate  (exchangeRate currency rate)) (floatToDecimalPlace(exchangeRate currency rate)) newcurrency 

floatToDecimalPlace :: Float -> Integer
floatToDecimalPlace f = truncate ((f-fromIntegral (truncate f))*10^2)

toEuro :: Currency -> Currency
toEuro currency
    | getCurrency currency == "Dollar" = exchangeCurrency currency 0.9 "Euro"
    | getCurrency currency == "Yen" = exchangeCurrency currency 0.0083 "Euro"
    | otherwise = currency
    

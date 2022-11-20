{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
data Currency = MkCurrency { -- Festlegung des Datentypen "Currency", bestehend aus "coins", "cents" und "currency"
    coins :: Integer, 
    cents :: Integer, 
    currency :: String
}

getCoins :: Currency -> Integer
getCoins (MkCurrency coins cents currency) = coins -- Methode nimmt 3 Elemente entgegen und gibt "coins" aus

getCents :: Currency -> Integer
getCents (MkCurrency coins cents currency) = cents -- Methode nimmt 3 Elemente entgegen und gibt "cents" aus

getCurrency :: Currency -> String
getCurrency (MkCurrency coins cents currency) = currency  -- Methode nimmt 3 Elemente entgegen und gibt "currency" aus

instance Show Currency where                                    -- Festlegung für die Anzeige der Geldbeträge mit einem Komma in Euro und Dollar (bzw. Yen, wenn keiner dieser beiden)
    show(MkCurrency coins cents currency) = 
        if currency == "Euro" || currency == "Dollar" 
        then show coins ++ "," ++ show cents ++ " " ++ currency
        else show coins ++ " " ++ currency

myEuro :: Currency
myEuro = MkCurrency 12 03 "Euro"      -- Funktionen erstellen jeweils Beispiele für Euro, Dollar und Yen aus dem Datentypen "Currency"

myDollar :: Currency
myDollar = MkCurrency 14 60 "Dollar"

myYen :: Currency
myYen = MkCurrency 120 0 "Yen"

currencyToFloat :: Currency -> Float -- durch "Currency" werden zwei Integer-Werte genommen und wandelt diese in einen Float-Wert um
currencyToFloat currency = fromIntegral (getCoins currency) + fromIntegral (getCents currency)/(10^2)

exchangeRate :: Currency -> Float -> Float --  der Currency-Wert wird durch die Rate dividiert, sofern es größer/gleich 1.0 ist und mit der Rate multipliziert, wenn nicht; in beiden wird ein Float-Wert wird am Ende ausgegeben => Wechselkurs
exchangeRate currency rate = if rate >= 1.0 then currencyToFloat currency/rate else currencyToFloat currency*rate

exchangeCurrency :: Currency -> Float -> String -> Currency -- es wird ein Currency-, ein Float- und ein Stringe- eingegeben, welche mit der unteren Formel "floatToDecimalPlace" verrechnet wird; damit wird eine Währung in eine andere umgerechnet; dasErgebnis ist wieder ein Currency-Wert
exchangeCurrency currency rate newcurrency = MkCurrency (truncate  (exchangeRate currency rate)) (floatToDecimalPlace(exchangeRate currency rate)) newcurrency 

floatToDecimalPlace :: Float -> Integer -- Umrechnung von Float-Werten in Integer-Werten
floatToDecimalPlace f = truncate ((f-fromIntegral (truncate f))*10^2)

toEuro :: Currency -> Currency
toEuro currency
    | getCurrency currency == "Dollar" = exchangeCurrency currency 0.9 "Euro"
    | getCurrency currency == "Yen" = exchangeCurrency currency 0.0083 "Euro"
    | otherwise = currency
    

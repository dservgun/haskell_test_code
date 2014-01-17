module Money where

   
data Currency = Currency Code Rational
            deriving (Eq, Ord, Show)

-- How to model a value?    
type Code = String
type Value = Rational
type Rate = Rational

roundingError = 1/100000

class Money a where
    
    currency :: Code -> Value-> a
    -- |A function to return the universal exchange currency
    -- usually when the rates are not available for both sides of the
    -- currency pair
    universal :: a
    -- |Convert a currency from a source to a target
    -- Converting from c1 ->  c2
    -- c1 = AUD, c2 = USD. and audusd = .87
    -- if we get  100 AUD -> 87 USD.
    convert :: (a, a) -> Value -> a
    inverse :: (a, a) -> Value -> a
    exchangeRate :: (a, a) -> Rate
    -- | A general rule that may or may not always work is that 
    -- inverse of conversion or conversion functions may not return the same result.
    -- For example if we want to convert AUD -> GBP and we have 
    -- AUD -> USD and USD -> GBP then theoretically,
    -- AUDGBP = AUDUSD * USDGBP. In this example, USD is the universal
    -- currency.
    -- The invariant for the currency pair should be something as below:
    -- AUDGBP = inverse of GBPAUD
    -- AUDGBP * GBPAUD = 1?
    -- Since these are floats, they should be less eq an epsilon.
    invariant :: a -> a -> Bool
    invariant a1 a2 = (exchangeRate (a1 ,a2)) * (exchangeRate (a2, a1)) < roundingError
    
    
    
instance Money Currency where 
    universal = Currency "USD" 1.0000
    currency aCode aValue = Currency aCode aValue
    convert (a@(Currency c1 v1) , b@(Currency c2 v))rate = Currency c2 $ rate * v1    
    inverse (a@(Currency c1 v1) , b@(Currency c2 v))rate2 = convert (b, a) rate2
    -- This function needs to query the database for the exchange rate
    -- for the pair and do something.
    exchangeRate (a,b) = 1.0

    

    

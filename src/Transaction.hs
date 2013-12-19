module Transaction where

data AccountType = Asset | Liability
type Name = String
type Amount = Int
data Account = Account Name AccountType
data Transaction = Transaction {
    source:: Account,
    destination :: Account,
    amount :: Amount
}


{- We want to model a double entry book keeeping system
 general ledger is a set of transactions sorted by date. 
 We want a set of functions that work on a Journal.
 Our initial motivation is to model the Journal as a monad
 allowing us to chain transactions.
 The chaining function would return the the list of transactions
 and the total liability of asset as a result of the transactions.
 This could be considered as a prototype of a simple journaling or
event sourcing system.
-} 

computeSnap :: Transaction -> (AccountType, Amount)
computeSnap (Transaction (Account sName sType) (Account dName dType) amount) = 
    if sName == dName then 
        (Asset, 0)
    else
        case (sType, dType) of
            (Asset, Liability) -> (Liability, amount)
            (Asset, Asset)    -> (Asset, amount)
            (Liability, Liability) -> (Liability, amount)
            (Liability, Asset) -> (Asset, amount)

computeSnapshot :: [Transaction] -> [(AccountType, Amount)]
{- This is an issue because when there are no transactions we 
probably need to do something meaningful.
-}
computeSnapshot [] = [(Asset, 0)]
computeSnapshot aList = map computeSnap aList

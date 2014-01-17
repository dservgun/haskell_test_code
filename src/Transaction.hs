module Transaction where

data AccountType = Debit | Credit
type Name = String
type Amount = Rational
data Account = Account Name AccountType Amount
data Transaction = Transaction {
    source:: Account,
    destination :: Account,
    amount :: Amount
}


type Journal =  [Transaction]
currentBalance (Account _ _ amount) = amount

valid :: Journal -> Bool
valid aJournal = 
        let 
           zero = 0/10000
           totalDebit = foldr (\t a -> a + currentBalance (source t)) zero aJournal
           totalCredit = foldr (\t a  -> a + currentBalance (destination t)) zero aJournal
        in
          abs(totalCredit + totalDebit) < zero
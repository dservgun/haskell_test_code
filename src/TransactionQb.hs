module TransactionQb 
(Transaction,
    dummyNumber,
    dummyAmount,
    prototype,
    createTransaction,
    SalesDetail,
    dateFromToday,
    TransactionType,
    allSales,
    allPurchases,
    allBankingTransactions, 
    salesRefund
)
where
import Data.Text (Text)
import Data.Time
import Data.Maybe

data Date = UTCTime
    deriving (Eq, Ord, Show, Read)
data Number = Double
    deriving (Eq, Ord, Show, Read)
data SalesTaxType = TaxPayment | TaxAdjustment
    deriving (Eq, Ord, Show,Read)
data Transaction = Transaction TransactionType Date Number Name Amount
    deriving (Eq, Ord, Show, Read)
data TransactionType = Sales SalesDetail | Purchase PurchaseDetail | Banking BankingDetail
    deriving (Eq, Ord, Show, Read)
    
data BankingDetail = SalesTax SalesTaxType | Check | CreditCard | JournalEntry | Deposit | CashExpense
    deriving (Eq, Ord, Show, Read)
data SalesDetail = Refund | Estimate | SalesReceipt | CreditMemo | Invoice | Payment
    deriving (Eq, Ord, Show, Read)
data PurchaseDetail = Expenses | Bill PaymentType | CashPurchase
    deriving (Eq, Ord, Show, Read)
data PaymentType = CheckDetail Bank CheckNumber Amount | CreditCardDetail Card Amount
    deriving (Eq, Ord, Show, Read)
type Name = String
type ABA = String
type AccountNumber = String
type InterestRate = Double
type CheckNumber = String
type Cash = Amount
type Amount = Double

data Card = Mastercard | Visa | AMEX    
    deriving (Eq, Ord, Show, Read)
data BankAccountType = CheckingAccount InterestRate | SavingsAccount InterestRate | MoneyMarket InterestRate
    deriving (Eq, Ord, Show, Read)
data Bank = Bank Name ABA BankAccountType AccountNumber
    deriving (Eq, Ord, Show, Read)
type Journal = [Maybe Transaction]

dateFromToday = read "2013-12-23"
dummyNumber = read "12.3"
dummyAmount = read "12.3"

prototype = "Prototype"
filterT :: Maybe Transaction -> Maybe Transaction -> Bool
filterT (Just (Transaction aType _ _ _ _ )) (Just(Transaction anotherType _ _ _ _)) =
      aType == anotherType
filterT Nothing (Just t) = False
filterT (Just t) Nothing = False

allSales :: Journal -> Journal
allSales [] = []
allSales aList 
    = filter (filterT proto) aList
        where proto = Just (Transaction (Sales Refund) dateFromToday dummyNumber prototype dummyAmount)



allPurchases:: Journal -> Journal
allPurchases [] = []
allPurchases aList     = filter (filterT proto) aList
        where proto = Just (Transaction (Purchase Expenses) dateFromToday dummyNumber prototype dummyAmount)

allBankingTransactions :: Journal -> Journal
allBankingTransactions [] = []
allBankingTransactions aList = filter (filterT proto) aList
    where proto = Just (Transaction (Banking JournalEntry) dateFromToday dummyNumber prototype dummyAmount)

salesRefund = Sales Refund
createTransaction :: TransactionType -> Date -> Number -> Name -> Amount -> Transaction
createTransaction aType aDate aNumber aName anAmount = Transaction aType aDate aNumber aName anAmount

totalSales :: Journal -> Maybe Transaction
totalSales [] = Nothing
totalSales aJournal = foldr addTransaction Nothing sales
    where sales  = allSales aJournal

totalPurchases :: Journal -> Maybe Transaction
totalPurchases [] = Nothing
totalPurchases aJournal = foldr addTransaction Nothing purchases
      where purchases = allPurchases aJournal

totalBankingTransactions :: Journal -> Maybe Transaction
totalBankingTransactions [] = Nothing
totalBankingTransactions aJournal = foldr addTransaction Nothing bankingTransactions
    where bankingTransactions = allBankingTransactions aJournal

addTransaction:: Maybe Transaction -> Maybe Transaction -> Maybe Transaction
addTransaction Nothing Nothing = Nothing
addTransaction a Nothing = a
addTransaction Nothing a = a
-- The values a b and c are place holders..we are losing the date for the transaction in this
-- example. Not sure if this will work for long. The design has 
-- quite a few issues: the number of functions seem to multiply with the type of transactions
-- we need to probably derive from Enum to limit the search functions??
addTransaction (Just (Transaction aType a b c anAmount)) (Just (Transaction anotherType _ _ _ anotherAmount))
    | aType == anotherType = Just (Transaction aType a b c (anAmount + anotherAmount))

         
         
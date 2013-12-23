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
type Journal = [Transaction]

dateFromToday = (read "2013-12-23")
dummyNumber = read "12.3"
dummyAmount = read "12.3"

prototype = "Prototype"
filterT (Transaction aType _ _ _ _ )(Transaction anotherType _ _ _ _) =
      aType == anotherType
allSales :: Journal -> Journal
allSales [] = []
allSales aList 
    = filter (filterT proto) aList
        where proto = Transaction (Sales Refund) dateFromToday dummyNumber prototype dummyAmount


allPurchases:: Journal -> Journal
allPurchases [] = []
allPurchases aList     = filter (filterT proto) aList
        where proto = Transaction (Purchase Expenses) dateFromToday dummyNumber prototype dummyAmount

allBankingTransactions :: Journal -> Journal
allBankingTransactions [] = []
allBankingTransactions aList = filter (filterT proto) aList
    where proto = Transaction (Banking JournalEntry) dateFromToday dummyNumber prototype dummyAmount              

salesRefund = Sales Refund          
createTransaction :: TransactionType -> Date -> Number -> Name -> Amount -> Transaction
createTransaction aType aDate aNumber aName anAmount = Transaction aType aDate aNumber aName anAmount

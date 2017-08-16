type CustomerId = Integer
type CardHolder = String
type CardNumber = String
type Address = String

data BillingInfo = CreditCard CardNumber CardHolder Address
    | CashOnDelivery
    | Invoice CustomerId
    deriving (Show)

data Book = Book Integer String [String]

bookId :: Book -> Integer
bookId      (Book id _ _) = id

bookTitle :: Book -> String
bookTitle   (Book _ title _) = title

bookAuthors :: Book -> [String]
bookAuthors   (Book _ _ authors) = authors

data Customer = Customer {
        customerId      ::  CustomerId
    ,   customerName    ::  String
    ,   customerAddress ::  Address   
} deriving (Show)

customer :: Customer
customer = Customer {
    customerId = 123
  , customerName = "John Doe"
  , customerAddress = "Planet Earth"
}
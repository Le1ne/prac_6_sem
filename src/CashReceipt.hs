module CashReceipt
( Product,
  CartItem, 
  BonusCard,
  Products,
  Cart,
  findProductPrice,
  calculateItemCost,
  calculateTotal,
  calculateDiscount,
  calculateFinalTotal
) where

import Data.List (lookup)

newtype Name = Name String
newtype Price = Price Double
newtype Category = Category String
newtype Quantity = Quantity Int
newtype Discount = Discount Double

instance Show Price where
    show (Price price) = show price

instance Show Quantity where
    show (Quantity quantity) = show quantity

instance Show Name where
    show (Name name) = show name

instance Show Discount where
    show (Discount discount) = show discount

instance Show Category where
    show (Category category) = show category

instance Eq Name where
    (Name name1) == (Name name2) = name1 == name2

instance Enum Quantity where
    toEnum n = Quantity (toEnum n)
    fromEnum (Quantity q) = fromEnum q

instance Eq Quantity where
    (Quantity a) == (Quantity b) = a == b
    (Quantity a) /= (Quantity b) = a /= b

instance Ord Quantity where
    compare (Quantity a) (Quantity b) = compare a b

instance Num Quantity where
    (Quantity a) + (Quantity b) = Quantity (a + b)
    (Quantity a) * (Quantity b) = Quantity (a * b)
    abs (Quantity a) = Quantity (abs a)
    signum (Quantity a) = Quantity (signum a)
    fromInteger n = Quantity (fromInteger n)
    (Quantity a) - (Quantity b) = Quantity (a - b)

instance Real Quantity where
    toRational (Quantity q) = toRational q

instance Integral Quantity where
    toInteger (Quantity q) = toInteger q
    quotRem (Quantity a) (Quantity b) = let (q, r) = quotRem a b in (Quantity q, Quantity r)

data Product = Product Name Price Category deriving (Show)
data CartItem = CartItem Name Quantity deriving (Show)
data BonusCard = BonusCard { birthday :: Maybe String, discountRate :: Discount } deriving (Show)

type Products = [Product]
type Cart = [CartItem]

getPrice :: Price -> Double
getPrice (Price p) = p

getDiscount :: Discount -> Double
getDiscount (Discount d) = d

findProductPrice :: Name -> Products -> Maybe Price
findProductPrice name products = lookup name [(productName, price) | Product productName price _ <- products]

calculateItemCost :: CartItem -> Products -> Maybe Double
calculateItemCost (CartItem name quantity) products = fmap ((* fromIntegral quantity) . getPrice) (findProductPrice name products)

calculateTotal :: Cart -> Products -> Maybe Double
calculateTotal cart products = sum <$> sequence (map (`calculateItemCost` products) cart)

calculateDiscount :: Double -> BonusCard -> Double
calculateDiscount total (BonusCard _ discountRate) = total * getDiscount(discountRate) / 100

calculateFinalTotal :: Double -> Double -> Double
calculateFinalTotal total discount = total - discount

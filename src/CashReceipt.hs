module CashReceipt
    ( findProductPrice
      calculateItemCost
      calculateTotal
      calculateDiscount
      calculateFinalTotal
    ) where

type Name = String
type Price = Double
type Category = String
type Quantity = Int
type Discount = Double

data Product = Product Name Price Category deriving (Show)
data CartItem = CartItem Name Quantity deriving (Show)
data BonusCard = BonusCard { birthday :: Maybe String, discountRate :: Discount } deriving (Show)

type Products = [Product]
type Cart = [CartItem]

findProductPrice :: Name -> Products -> Maybe Price

calculateItemCost :: CartItem -> Products -> Maybe Double

calculateTotal :: Cart -> Products -> Maybe Double

calculateDiscount :: Double -> BonusCard -> Double

calculateFinalTotal :: Double -> Double -> Double

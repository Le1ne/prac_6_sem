{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype Name = Name String deriving (Eq, Show)
newtype Price = Price Double deriving (Show)
newtype Category = Category String deriving (Show)
newtype Quantity = Quantity Int deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
newtype Discount = Discount Double deriving (Show)
newtype Products = Products [Product] deriving (Show)
newtype Cart = Cart [CartItem] deriving (Show)

minDiscountRate, maxDiscountRate, minPurchaseAmount :: Double
minDiscountRate = 1
maxDiscountRate = 7
minPurchaseAmount = 3000

negativePriceError :: String
negativePriceError = "Цена товара не может быть отрицательной."

invalidDiscountError :: String
invalidDiscountError = "Значение скидки должно быть между 0 и 100 процентами."

negativeQuantityError :: String
negativeQuantityError = "Количество товара не может быть отрицательным."

purchaseAmountError :: String
purchaseAmountError = "Сумма покупок не может быть отрицательной."

discountRangeError :: String
discountRangeError = "Скидка по бонусной карте применяется при значении от 1% до 7%."

getPrice :: Price -> Double
getPrice (Price p)
  | p < 0 = error negativePriceError
  | otherwise = p

findProductPrice :: Name -> Products -> Maybe Price
findProductPrice name (Products products) = lookup name [(productName, price) | Product productName price _ <- products]

calculateItemCost :: CartItem -> Products -> Maybe Double
calculateItemCost (CartItem name (Quantity quantity)) products
  | quantity < 0 = error negativeQuantityError
  | otherwise = fmap ((* fromIntegral quantity) . getPrice) (findProductPrice name products)

calculateTotal :: Cart -> Products -> Maybe Double
calculateTotal (Cart cart) products = sum <$> sequence (map (`calculateItemCost` products) cart)

calculateDiscount :: Double -> BonusCard -> Double
calculateDiscount total (BonusCard _ (Discount discountRate))
  | total > minPurchaseAmount || (discountRate >= minDiscountRate && discountRate <= maxDiscountRate) = total * discountRate / 100
  | otherwise = 0

calculateFinalTotal :: Double -> Double -> Double
calculateFinalTotal total discount = total - discount

data Product = Product Name Price Category deriving (Show)
data CartItem = CartItem Name Quantity deriving (Show)
data BonusCard = BonusCard { birthday :: Maybe String, discountRate :: Discount } deriving (Show)

--BonusCard 1-7%, if total > 3000 => discount--
--to consts--
--Нужно добавить обработку ошибок (разных, например, чтение файла, проблемы с файлами, проблемы с числами, обработку в другой файл)--
--Main не должен содержать парсера--
--Убрать инстансы по комментарию выше--
--Ошибки в отдельные константы--

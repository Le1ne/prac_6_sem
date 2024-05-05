{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Модуль CashReceipt, определяющий структуры данных и функции для работы с чеками кассы
module CashReceipt
( Product(..),
  CartItem(..), 
  BonusCard(..),
  Products(..),
  Cart(..),
  Name(..),
  Price(..),
  Category(..), 
  Quantity(..),
  Discount(..),
  findProductPrice,
  calculateItemCost,
  calculateTotal,
  calculateDiscount,
  calculateFinalTotal,
  minDiscountRate, 
  maxDiscountRate
) where

import Constants (minDiscountRate, maxDiscountRate, minPurchaseAmount, defaultPurchaseDiscount, negativePriceError, invalidDiscountError, negativeQuantityError, purchaseAmountError)

-- Объявления новых типов, используемых для представления различных аспектов товаров
newtype Name = Name String deriving (Eq, Show)
newtype Price = Price Double deriving (Show)
newtype Category = Category String deriving (Show)
newtype Quantity = Quantity Int deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
newtype Discount = Discount Double deriving (Show)
newtype Products = Products [Product] deriving (Show)
newtype Cart = Cart [CartItem] deriving (Show)

-- Определения типов данных для товаров в магазине, корзины и бонусных карт.
data Product = Product Name Price Category deriving (Show)
data CartItem = CartItem Name Quantity deriving (Show)
data BonusCard = BonusCard { birthday :: Maybe String, discountRate :: Discount } deriving (Show)

-- Получение цены из типа Price с проверкой на отрицательное значение
getPrice :: Price -> Double
getPrice (Price p)
  | p < 0 = error negativePriceError -- Вывод ошибки при отрицательной цене
  | otherwise = p

-- Функция поиска цены продукта по имени в списке продуктов
findProductPrice :: Name -> Products -> Maybe Price
findProductPrice name (Products products) = lookup name [(productName, price) | Product productName price _ <- products]

-- Расчет стоимости позиции в корзине на основе количества и цены
calculateItemCost :: CartItem -> Products -> Maybe Double
calculateItemCost (CartItem name (Quantity quantity)) products
  | quantity < 0 = error negativeQuantityError -- Обработка отрицательного количества товаров
  | otherwise = fmap ((* fromIntegral quantity) . getPrice) (findProductPrice name products)

-- Расчет общей стоимости корзины
calculateTotal :: Cart -> Products -> Maybe Double
calculateTotal (Cart cart) products = do
  let total = sum <$> sequence (map (`calculateItemCost` products) cart)
  case total of
    Just t | t < 0 -> error purchaseAmountError
    _ -> total

-- Расчет скидки в зависимости от наличия бонусной карты и суммы покупки
calculateDiscount :: Double -> Maybe BonusCard -> Double
calculateDiscount total Nothing
  | total > minPurchaseAmount = (defaultPurchaseDiscount / 100) * total -- Скидка по умолчанию при достаточной сумме
  | otherwise = 0
calculateDiscount total (Just (BonusCard _ (Discount localDiscountRate)))
  | localDiscountRate < 0 || localDiscountRate > 100 = error invalidDiscountError
  | total > minPurchaseAmount = max ((defaultPurchaseDiscount / 100) * total) ((localDiscountRate / 100) * total)
  | otherwise = (localDiscountRate / 100) * total

-- Расчет итоговой суммы с учетом скидки
calculateFinalTotal :: Double -> Double -> Double
calculateFinalTotal total discount = total - discount

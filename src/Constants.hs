module Constants
( minDiscountRate,
  maxDiscountRate,
  minPurchaseAmount,
  defaultPurchaseDiscount,
  negativePriceError,
  invalidDiscountError,
  negativeQuantityError,
  purchaseAmountError,
  errorLoadingProducts,
  errorLoadingCart,
  errorLoadingBonusCard,
  welcomeMessage,
  promptProductsFile,
  promptCartFile,
  promptBonusCardFile,
  promptOutputFile,
  outOfBoundsError,
  errorReadingFile
) where

minDiscountRate, maxDiscountRate, minPurchaseAmount, defaultPurchaseDiscount :: Double
minDiscountRate = 1
maxDiscountRate = 7
minPurchaseAmount = 3000
defaultPurchaseDiscount = 5

negativePriceError :: String
negativePriceError = "Цена товара не может быть отрицательной."

invalidDiscountError :: String
invalidDiscountError = "Значение скидки должно быть между 0 и 100 процентами."

negativeQuantityError :: String
negativeQuantityError = "Количество товара не может быть отрицательным."

purchaseAmountError :: String
purchaseAmountError = "Сумма покупок не может быть отрицательной."

errorLoadingProducts :: String
errorLoadingProducts = "Ошибка загрузки списка продуктов: "

errorLoadingCart :: String
errorLoadingCart = "Ошибка загрузки корзины: "

errorLoadingBonusCard :: String
errorLoadingBonusCard = "Ошибка загрузки бонусной карты: "

welcomeMessage :: String
welcomeMessage = "Добро пожаловать в приложение 'Кассовый аппарат'"

promptProductsFile :: String
promptProductsFile = "Пожалуйста, введите путь к файлу с продуктами:"

promptCartFile :: String
promptCartFile = "Пожалуйста, введите путь к файлу с корзиной:"

promptBonusCardFile :: String
promptBonusCardFile = "Пожалуйста, введите путь к файлу с бонусной картой (оставьте пустым, если её нет):"

promptOutputFile :: String
promptOutputFile = "Пожалуйста, введите путь к файлу для сохранения результатов:"

outOfBoundsError :: String
outOfBoundsError = "Скидка по бонусной карте должна быть в пределах от 1% до 7%."

errorReadingFile :: String
errorReadingFile = "Ошибка чтения файла: "

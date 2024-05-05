-- Модуль Parser, определяющий функции для обработки данных из файлов
module Parser
( readProducts,
  readCart,
  readBonusCard,
  parseProduct,
  parseCartItem,
  parseBonusCard
) where

import CashReceipt (Product(..), CartItem(..), BonusCard(..), Products(..), Cart(..), Name(..), Price(..), Quantity(..), Discount(..), Category(..), minDiscountRate, maxDiscountRate)
import System.IO()
import System.Directory (doesFileExist)
import Control.Exception
import Data.Maybe (catMaybes)
import Constants (outOfBoundsError, errorReadingFile)

-- Разбирает строку на данные продукта, используя разделитель ','
parseProduct :: String -> Maybe Product
parseProduct str = 
    case wordsWhen (==',') str of
        [name, priceStr, category] -> Just $ Product (Name name) (Price (read priceStr)) (Category category)
        _ -> Nothing -- Возвращает Nothing, если строка не соответствует ожидаемому формату

-- Разбирает строку на данные элемента корзины, используя разделитель ','
parseCartItem :: String -> Maybe CartItem
parseCartItem str = 
    case wordsWhen (==',') str of
        [name, quantityStr] -> Just $ CartItem (Name name) (Quantity (read quantityStr))
        _ -> Nothing -- Возвращает Nothing, если строка не соответствует ожидаемому формату

-- Разбирает строку на данные бонусной карты, проверяя допустимый диапазон скидки
parseBonusCard :: String -> Maybe BonusCard
parseBonusCard str = 
    case wordsWhen (==',') str of
        [birthdayStr, discountRateStr] ->
            let discount = read discountRateStr
            in if discount >= minDiscountRate && discount <= maxDiscountRate
               then Just $ BonusCard (if null birthdayStr then Nothing else Just birthdayStr) (Discount discount)
               else Nothing -- Возвращает Nothing, если скидка вне допустимого диапазона
        _ -> Nothing

-- Вспомогательная функция для разделения строки по предикату
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

-- Чтение файла продуктов и преобразование его содержимого в список товаров
readProducts :: FilePath -> IO (Either String Products)
readProducts filePath =
    tryReadFile filePath >>= return . fmap (Products . catMaybes . map parseProduct . lines)

-- Чтение файла корзины и преобразование его содержимого в список элементов корзины
readCart :: FilePath -> IO (Either String Cart)
readCart filePath =
    tryReadFile filePath >>= return . fmap (Cart . catMaybes . map parseCartItem . lines)

-- Чтение файла бонусной карты и преобразование его содержимого в данные бонусной карты
readBonusCard :: FilePath -> IO (Either String (Maybe BonusCard))
readBonusCard filePath = do
  exists <- doesFileExist filePath
  if exists
  then do
    content <- tryReadFile filePath
    return $ case content of
      Right text -> case parseBonusCard text of
                     Just card -> Right (Just card)
                     Nothing -> Left outOfBoundsError -- Ошибка, если скидка не соответствует допустимому диапазону
      Left err -> Left err
  else return $ Right Nothing -- Возвращает Nothing, если файл не существует

-- Вспомогательная функция для безопасного чтения файла с обработкой ошибок
tryReadFile :: FilePath -> IO (Either String String)
tryReadFile path = catch (Right <$> readFile path) handler
  where handler :: IOException -> IO (Either String String)
        handler e = return . Left $ errorReadingFile ++ show e -- Возвращает сообщение об ошибке при чтении файла

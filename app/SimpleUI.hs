-- Модуль SimpleUI, реализующий пользовательский интерфейс
module SimpleUI
( runUI
) where

import Parser (readProducts, readCart, readBonusCard)
import CashReceipt (calculateTotal, calculateDiscount, calculateFinalTotal, findProductPrice, CartItem(..), BonusCard(..), Products(..), Cart(..), Name(..), Price(..), Quantity(..), Discount(..))
import Data.Maybe (fromMaybe)
import Constants (errorLoadingProducts, errorLoadingCart, errorLoadingBonusCard, welcomeMessage, promptProductsFile, promptCartFile, promptBonusCardFile, promptOutputFile)

-- Основная функция интерфейса пользователя
runUI :: IO ()
runUI = do
    putStrLn welcomeMessage -- Вывод приветственного сообщения
    putStrLn promptProductsFile
    productsPath <- getLine -- Ввод пути к файлу продуктов
    putStrLn promptCartFile
    cartPath <- getLine -- Ввод пути к файлу корзины
    putStrLn promptBonusCardFile
    bonusCardPath <- getLine -- Ввод пути к файлу бонусной карты
    putStrLn promptOutputFile
    outputFile <- getLine -- Ввод пути к файлу для вывода результатов

    productsResult <- readProducts productsPath
    case productsResult of
        Left err -> putStrLn $ errorLoadingProducts ++ err -- Обработка ошибки загрузки продуктов
        Right products -> do
            cartResult <- readCart cartPath
            case cartResult of
                Left err -> putStrLn $ errorLoadingCart ++ err -- Обработка ошибки загрузки корзины
                Right cart -> do
                    bonusCardResult <- readBonusCard bonusCardPath
                    case bonusCardResult of
                        Left err -> putStrLn $ errorLoadingBonusCard ++ err -- Обработка ошибки загрузки бонусной карты
                        Right maybeBonusCard -> do
                            let bonusCardApplied = fromMaybe (BonusCard Nothing (Discount 0)) maybeBonusCard
                            let total = calculateTotal cart products
                            let discount = calculateDiscount (fromMaybe 0 total) (Just bonusCardApplied)
                            let finalTotal = calculateFinalTotal (fromMaybe 0 total) discount
                            let itemsDetails = concatMap (formatCartItem products) (getCartItems cart)
                            let csvData = "Наименование товара,Количество,Стоимость за штуку,Сумма\n" ++
                                          itemsDetails ++
                                          "Скидка,,," ++ show discount ++ "\n" ++
                                          "Итого,,," ++ show finalTotal ++ "\n"
                            writeFile outputFile csvData -- Запись результатов в файл
                            putStrLn $ "Результаты сохранены в файл: " ++ outputFile

-- Форматирует данные о покупке в строку для CSV файла
formatCartItem :: Products -> CartItem -> String
formatCartItem (Products products) (CartItem (Name name) (Quantity quantity)) =
    case findProductPrice (Name name) (Products products) of
        Just (Price price) ->
            name ++ ","
            ++ show quantity ++ ","
            ++ show price ++ ","
            ++ show (price * fromIntegral quantity) ++ "\n"
        Nothing -> "Товар не найден.\n"

-- Получает список товаров в корзине
getCartItems :: Cart -> [CartItem]
getCartItems (Cart items) = items

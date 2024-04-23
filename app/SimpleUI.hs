module SimpleUI
( runUI
) where

import Parser (readProducts, readCart, readBonusCard)
import CashReceipt (calculateTotal, calculateDiscount, calculateFinalTotal, BonusCard(..), Discount(..))
import Data.Maybe (fromMaybe)

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

runUI :: IO ()
runUI = do
    putStrLn welcomeMessage
    putStrLn promptProductsFile
    productsPath <- getLine
    putStrLn promptCartFile
    cartPath <- getLine
    putStrLn promptBonusCardFile
    bonusCardPath <- getLine
    productsResult <- readProducts productsPath
    case productsResult of
        Left err -> putStrLn $ errorLoadingProducts ++ err
        Right products -> do
            cartResult <- readCart cartPath
            case cartResult of
                Left err -> putStrLn $ errorLoadingCart ++ err
                Right cart -> do
                    bonusCardResult <- readBonusCard bonusCardPath
                    case bonusCardResult of
                        Left err -> putStrLn $ errorLoadingBonusCard ++ err
                        Right maybeBonusCard -> do
                            let bonusCardApplied = fromMaybe (BonusCard Nothing (Discount 0)) maybeBonusCard
                            case calculateTotal cart products of
                                Just total -> do
                                    let discount = calculateDiscount total bonusCardApplied
                                    let finalTotal = calculateFinalTotal total discount
                                    putStrLn $ "Итог без скидки: " ++ show total
                                    putStrLn $ "Скидка: " ++ show discount
                                    putStrLn $ "Итог с учётом скидки: " ++ show finalTotal
                                Nothing -> putStrLn "Ошибка подсчёта суммы покупок"

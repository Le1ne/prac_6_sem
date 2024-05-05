module SimpleUI
( runUI
) where

import Parser (readProducts, readCart, readBonusCard)
import CashReceipt (calculateTotal, calculateDiscount, calculateFinalTotal, findProductPrice, CartItem(..), BonusCard(..), Products(..), Cart(..), Name(..), Price(..), Quantity(..), Discount(..))
import Data.Maybe (fromMaybe)
import Constants (errorLoadingProducts, errorLoadingCart, errorLoadingBonusCard, welcomeMessage, promptProductsFile, promptCartFile, promptBonusCardFile, promptOutputFile)

runUI :: IO ()
runUI = do
    putStrLn welcomeMessage
    putStrLn promptProductsFile
    productsPath <- getLine
    putStrLn promptCartFile
    cartPath <- getLine
    putStrLn promptBonusCardFile
    bonusCardPath <- getLine
    putStrLn promptOutputFile
    outputFile <- getLine

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
                            let total = calculateTotal cart products
                            let discount = calculateDiscount (fromMaybe 0 total) (Just bonusCardApplied)
                            let finalTotal = calculateFinalTotal (fromMaybe 0 total) discount
                            let itemsDetails = concatMap (formatCartItem products) (getCartItems cart)
                            let csvData = "Наименование товара,Количество,Стоимость за штуку,Сумма\n" ++
                                          itemsDetails ++
                                          "Скидка,,," ++ show discount ++ "\n" ++
                                          "Итого,,," ++ show finalTotal ++ "\n"
                            writeFile outputFile csvData
                            putStrLn $ "Результаты сохранены в файл: " ++ outputFile

formatCartItem :: Products -> CartItem -> String
formatCartItem (Products products) (CartItem (Name name) (Quantity quantity)) =
    case findProductPrice (Name name) (Products products) of
        Just (Price price) ->
            name ++ ","
            ++ show quantity ++ ","
            ++ show price ++ ","
            ++ show (price * fromIntegral quantity) ++ "\n"
        Nothing -> "Product not found.\n"

getCartItems :: Cart -> [CartItem]
getCartItems (Cart items) = items

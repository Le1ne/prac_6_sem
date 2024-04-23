module SimpleUI where

import Parser (readProducts, readCart, readBonusCard)
import CashReceipt (calculateTotal, calculateDiscount, calculateFinalTotal, Products, Cart, BonusCard, Product, CartItem, Discount, Name)
import Data.Maybe (fromMaybe)

errorLoadingProducts :: String
errorLoadingProducts = "Error loading products: "

errorLoadingCart :: String
errorLoadingCart = "Error loading cart: "

errorLoadingBonusCard :: String
errorLoadingBonusCard = "Error loading bonus card: "

welcomeMessage :: String
welcomeMessage = "Welcome to the Simple Cash Receipt Application"

promptProductsFile :: String
promptProductsFile = "Please, enter the path to the products file:"

promptCartFile :: String
promptCartFile = "Please, enter the path to the cart file:"

promptBonusCardFile :: String
promptBonusCardFile = "Please, enter the path to the bonus card file (leave empty if none):"

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
                        Right bonusCard -> do
                            let total = calculateTotal cart products
                            let bonusCardApplied = fromMaybe (BonusCard Nothing (Discount 0)) bonusCard
                            let discount = calculateDiscount total bonusCardApplied
                            let finalTotal = calculateFinalTotal total discount
                            putStrLn $ "Total without discount: " ++ show total
                            putStrLn $ "Discount: " ++ show discount
                            putStrLn $ "Final Total: " ++ show finalTotal

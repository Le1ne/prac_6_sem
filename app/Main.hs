module Main (main) where

import CashReceipt (Product, CartItem, BonusCard, Products, Cart)
import System.IO
import Control.Monad
import Data.Maybe
import System.Directory (doesFileExist)

parseProduct :: String -> Product
parseProduct str = let [name, priceStr, category] = wordsWhen (==',') str
                   in Product name (read priceStr) category

parseCartItem :: String -> CartItem
parseCartItem str = let [name, quantityStr] = wordsWhen (==',') str
                    in CartItem name (read quantityStr)

parseBonusCard :: String -> BonusCard
parseBonusCard str = let [birthdayStr, discountRateStr] = wordsWhen (==',') str
                     in BonusCard (if null birthdayStr then Nothing else Just birthdayStr) (read discountRateStr)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

readProducts :: FilePath -> IO Products
readProducts filePath = do
	contents <- readFile filePath
	return $ map parseProduct (lines contents)

readCart :: FilePath -> IO Cart
readCart filePath = do
	contents <- readFile filePath
	return $ map parseCartItem (lines contents)

readBonusCard :: FilePath -> IO (Maybe BonusCard)
readBonusCard filePath = do
  	exists <- doesFileExist filePath
  	if exists
  	then fmap (Just . parseBonusCard) (readFile filePath)
  	else return Nothing

processFiles :: FilePath -> FilePath -> FilePath -> IO ()
processFiles productsPath cartPath bonusCardPath = do
	products <- readProducts productsPath
  	cart <- readCart cartPath
  	bonusCardOpt <- readBonusCard bonusCardPath
  	let bonusCard = fromMaybe (BonusCard Nothing 0) bonusCardOpt
  	printReceipt cart products bonusCard

main :: IO ()
main = do
  	let productsPath = "products.txt"
  	let cartPath = "cart.txt"
  	let bonusCardPath = "bonus_card.txt"
  	processFiles productsPath cartPath bonusCardPath

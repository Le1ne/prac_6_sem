module Parser
( readProducts,
  readCart,
  readBonusCard,
  parseProduct,
  parseCartItem,
  parseBonusCard
) where

import CashReceipt (Product, CartItem, BonusCard, Products, Cart)
import System.IO
import System.Directory (doesFileExist)
import Control.Exception

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

readProducts :: FilePath -> IO (Either String Products)
readProducts filePath = tryReadFile filePath >>= return . fmap (map parseProduct . lines)

readCart :: FilePath -> IO (Either String Cart)
readCart filePath = tryReadFile filePath >>= return . fmap (map parseCartItem . lines)

readBonusCard :: FilePath -> IO (Either String (Maybe BonusCard))
readBonusCard filePath = do
  exists <- doesFileExist filePath
  if exists
  then tryReadFile filePath >>= return . fmap (Just . parseBonusCard)
  else return $ Right Nothing

tryReadFile :: FilePath -> IO (Either String String)
tryReadFile path = catch (Right <$> readFile path) handler
  where handler :: IOException -> IO (Either String String)
        handler e = return . Left $ "Error reading file: " ++ show e

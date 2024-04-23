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
import Data.Maybe(catMaybes)

parseProduct :: String -> Maybe Product
parseProduct str = 
    case wordsWhen (==',') str of
        [name, priceStr, category] -> Just $ Product (Name name) (Price (read priceStr)) (Category category)
        _ -> Nothing

parseCartItem :: String -> Maybe CartItem
parseCartItem str = 
    case wordsWhen (==',') str of
        [name, quantityStr] -> Just $ CartItem (Name name) (Quantity (read quantityStr))
        _ -> Nothing

parseBonusCard :: String -> Maybe BonusCard
parseBonusCard str = 
    case wordsWhen (==',') str of
        [birthdayStr, discountRateStr] ->
            let discount = read discountRateStr
            in if discount >= minDiscountRate && discount <= maxDiscountRate
               then Just $ BonusCard (if null birthdayStr then Nothing else Just birthdayStr) (Discount discount)
               else Nothing
        _ -> Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

readProducts :: FilePath -> IO (Either String Products)
readProducts filePath =
    tryReadFile filePath >>= return . fmap (Products . catMaybes . map parseProduct . lines)

readCart :: FilePath -> IO (Either String Cart)
readCart filePath =
    tryReadFile filePath >>= return . fmap (Cart . catMaybes . map parseCartItem . lines)

readBonusCard :: FilePath -> IO (Either String (Maybe BonusCard))
readBonusCard filePath = do
  exists <- doesFileExist filePath
  if exists
  then do
    content <- tryReadFile filePath
    return $ case content of
      Right text -> case parseBonusCard text of
                     Just card -> Right (Just card)
                     Nothing -> Left "Скидка по бонусной карте должна быть в пределах от 1% до 7%."
      Left err -> Left err
  else return $ Right Nothing

tryReadFile :: FilePath -> IO (Either String String)
tryReadFile path = catch (Right <$> readFile path) handler
  where handler :: IOException -> IO (Either String String)
        handler e = return . Left $ "Error reading file: " ++ show e

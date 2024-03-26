module Main (main) where

import CashReceipt
import System.IO
import Control.Monad
import Data.Maybe

parseProduct :: String -> Product

parseCartItem :: String -> CartItem

parseBonusCard :: String -> BonusCard

wordsWhen :: (Char -> Bool) -> String -> [String]

eadProducts :: FilePath -> IO Products

readCart :: FilePath -> IO Cart

readBonusCard :: FilePath -> IO (Maybe BonusCard)

processFiles :: FilePath -> FilePath -> FilePath -> IO ()

main :: IO ()
main = someFunc

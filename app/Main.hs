-- Модуль Main, запускающий работу программы
module Main
( main
) where

import SimpleUI (runUI)

-- Запуск функции пользовательского интерфейса
main :: IO ()
main = runUI

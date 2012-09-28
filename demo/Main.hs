{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pin.Demo
import Data.Text
import System.Environment

main :: IO ()
main =
  getArgs >>= \args -> case args of
    (key : []) -> main' key
    _ -> putStrLn ("usage: pin-demo <your-test-api-key>" :: String)

main' :: String -> IO ()
main' key =
  demoCharge (pack key) >>= print

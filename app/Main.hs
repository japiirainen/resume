{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Reader (runReader)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Resume (resume)
import System.Environment (getArgs)
import TeX (Mode (..))

main ∷ IO ()
main =
  getArgs >>= \case
    ["en"] → f English
    ["elab"] → f Elaborated
    _ →
      putStrLn $
        unlines
          [ "Usage: <program> MODE"
          , "MODE can be en or elab"
          ]
  where
    f x = do
      setLocaleEncoding utf8
      putStrLn $ runReader resume x

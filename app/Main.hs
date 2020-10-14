{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative.Tools ((<.>))
import Data.Text (Text, empty, pack)
import Data.Text.IO (putStr, readFile)
import System.FilePath ((</>))
import qualified Y2019.D01 as Y19
import qualified Y2019.D02 as Y19
import qualified Y2019.D03 as Y19
import Prelude hiding (putStr, readFile)

execute :: Text -> (Text -> a) -> FilePath -> IO a
execute label day fp = do
  putStr $ label <> ": "
  day <.> readFile $ "./input/" </> fp

main :: IO ()
main = do
  print =<< execute "Y19/d01a" Y19.d01a "Y19/d01"
  print =<< execute "Y19/d01b" Y19.d01b "Y19/d01"
  print =<< execute "Y19/d02a" Y19.d02a "Y19/d02"
  print =<< execute "Y19/d02b" Y19.d02b "Y19/d02"
  print =<< execute "Y19/d03a" Y19.d03a "Y19/d03"

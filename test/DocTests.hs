{-# OPTIONS_GHC -fplugin Numeric.Units.Dimensional.DK.Solver #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import Data.List (isSuffixOf)
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doctest . filter (not . isSuffixOf "Solver.hs")
  -- TODO Figure out what is the problem with running doctest on Solver module.

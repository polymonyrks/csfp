{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches
    -fwarn-monomorphism-restriction -fwarn-missing-signatures #-}
-- {-# HLINT ignore "Functor law" #-}

module Main (main) where

import Lib
import Data.List ( nub, sort )
import System.IO
import Data.Char

main :: IO ()
main = someFunc


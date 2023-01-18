module Lib
    ( someFunc,
      cshowL, cshowIL
    ) where

import Text.Show.Unicode

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- utility functions

cshowIL :: Show b => [b] -> IO ()
cshowIL lis = mapM_ uprint $ zip [0 .. (length lis - 1)] lis

cshowL :: (Foldable t, Show a) => t a -> IO ()
cshowL = mapM_ uprint

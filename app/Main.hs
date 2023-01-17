module Main (main) where

import Lib
-- import Data.List

main :: IO ()
main = someFunc

data Sent = Sent NP VP deriving Show

data NP = SnowWhite | Alice | Dorothy | Goldilocks | LittleMook | Atreyu | Everyone | Someone | NP1 DET CN | NP2 DET RCN
   deriving Show

data DET = The | Every | Some | No | Most
  deriving Show

data CN = Girl | Boy | Princess | Dwarf | Giant| Wizard | Sword | Dagger
  deriving Show

data ADJ = Fake deriving Show

data RCN = RCN1 CN That VP | RCN2 CN That NP TV| RCN3 ADJ CN
  deriving Show

data That = That deriving Show

data VP = Laughed | Cheered | Shuddered| VP1 TV NP | VP2 DV NP NP| VP3 AV To INF
  deriving Show

data TV = Loved | Admired | Helped| Defeated | Caught
  deriving Show

data DV = Gave deriving Show

data AV = Hoped | Wanted
  deriving Show

data INF = Laugh | Cheer | Shudder | INF TINF NP
   deriving Show

data TINF = Love | Admire | Help | Defeat | Catch
  deriving Show

data To = To deriving Show

data Form = P String | Ng Form | Cnj [Form] | Dsj [Form]
  deriving Eq

instance Show Form where
  show (P name) = name
  show (Ng f) = '-' : show f
  show (Cnj fs) =  '&' : show fs
  show (Dsj fs) =  'v' : show fs

form1 , form2 :: Form
form1 = Cnj [P "p", Ng (P "p")]
form2 = Dsj [P "p1", P "p2", P "p3", P "p4"]















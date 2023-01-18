{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches
    -fwarn-monomorphism-restriction -fwarn-missing-signatures #-}
-- {-# HLINT ignore "Functor law" #-}

module Main (main) where

import Lib
import Data.List ( nub, sort )
-- import Data.Char (toUpper)
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

data VP = Laughed | Cheered | Shuddered | VP1 TV NP | VP2 DV NP NP| VP3 AV To INF
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

data Form =
  P String |
  Ng Form |
  Cnj [Form] |
  Dsj [Form]
   deriving Eq

instance Show Form where
  show (P name) = name
  show (Ng f) = '-' : show f
  show (Cnj fs) =  '&' : show fs
  show (Dsj fs) =  'v' : show fs

form1 , form2 :: Form
form1 = Cnj [P "p", Ng (P "p")]
form2 = Dsj [P "p1", P "p2", P "p3", P "p4"]


type Name = String
type Index = [Int]
data Variable = Variable Name Index
 deriving (Eq, Ord)


instance Show Variable where
  show (Variable name []) = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
     where
       showInts [] = ""
       showInts [i] = show i
       showInts (i:is') = show i ++ "_" ++ showInts is'


data Formula a =
  Atom String [a] |
  Eq a a |
  Neg (Formula a) |
  Impl (Formula a) (Formula a) |
  Equi (Formula a) (Formula a) |
  Conj [Formula a] |
  Disj [Formula a] |
  Forall Variable (Formula a) |
  Exists Variable (Formula a)
    deriving Eq

instance Show a => Show (Formula a) where
  show (Atom s []) = s
  show (Atom s xs) = s ++ show xs
  show (Eq t1 t2) = show t1 ++ "==" ++ show t2
  show (Neg form) = '~' : (show form)
  show (Impl f1 f2) = "(" ++ show f1 ++ "==>"++ show f2 ++ ")"
  show (Equi f1 f2) = "(" ++ show f1 ++ "<=>"++ show f2 ++ ")"
  show (Conj []) = "true"
  show (Conj fs) = "conj" ++ show fs
  show (Disj []) = "false"
  show (Disj fs) = "disj" ++ show fs
  show (Forall v f) = "A " ++ show v ++ (' ' : show f)
  show (Exists v f) = "E " ++ show v ++ (' ' : show f)

formula0 :: Formula Variable
formula0 = Atom "R" [x',y']
formula1 :: Formula Variable
formula1 = Forall x' (Atom "R" [x',x'])
formula2 :: Formula Variable
formula2 = Forall x' (Forall y' (Impl (Atom "R" [x',y']) (Atom "R" [y',x'])))

data Term = Var Variable | Struct String [Term]
  deriving (Eq, Ord)

instance Show Term where
  show (Var v) = show v
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts


x',y',z' :: Variable
x' = Variable "x" []
y' = Variable "y" []
z' = Variable "z" []


tx', ty', tz' :: Term
tx' = Var x'
ty' = Var y'
tz' = Var z'

isVar :: Term -> Bool
isVar (Var _) = True
isVar (Struct _ _) = False

varsInTerm :: Term -> [Variable]
varsInTerm (Var v) = [v]
varsInTerm  (Struct s ts) = (sort . nub . concatMap varsInTerm) ts

propNames :: Form -> [String]
propNames (P str) = [str]
propNames (Ng f) = propNames f
propNames (Cnj fs) = (sort . nub . concatMap propNames) fs
propNames (Dsj fs) = (sort . nub . concatMap propNames) fs

genVals :: [String] -> [[(String, Bool)]]
genVals [] = [[]]
genVals (name : names) = map ((name, True) :) (genVals names) ++ map ((name, False) :) (genVals names)

genVals' :: [String] -> [[(String, Bool)]]
genVals' xs = foldl f [[]] xs
  where
    f yss x = [ys ++ [xRes] | xRes <- [(x, True), (x, False)], ys <- yss]

allVals :: Form -> [[(String, Bool)]]
allVals f = genVals $ propNames f

eval :: [(String, Bool)] -> Form -> Bool
eval [] (P str) =  error ("no info about " ++ show str)
eval ((i,b):xs) (P str)
 | i == str = b
 | otherwise = eval' xs (P str)
eval vals (Ng fm) = not $ eval vals fm
eval vals (Cnj fms) = all (eval vals) fms
eval vals (Dsj fms) =  any (eval vals) fms

tautology :: Form -> Bool
tautology fm = and $ map (\vals -> eval vals fm) varsPatterns
  where
    varsPatterns = allVals fm

satisfiable :: Form -> Bool
satisfiable fm = or $ map (\vals -> eval vals fm) varsPatterns
  where
    varsPatterns = allVals fm

contradiction :: Form -> Bool
contradiction = not . satisfiable

implies :: Form -> Form -> Bool
implies fm0 fm1 = contradiction $ Cnj [fm0, Ng fm1]

update :: [[(String, Bool)]] -> Form -> [[(String, Bool)]]
update vals f = [v | v <- vals, eval v f]

eval' :: [(String, Bool)] -> Form -> Bool
eval' vals (P str)
  | 0 < length filtered = snd $ head filtered
  | otherwise = error ("no info about " ++ show str)
  where
    filtered = filter (\x -> fst x == str) vals
eval' vals (Ng fm) = not $ eval vals fm
eval' vals (Cnj fms) = and $ map (eval vals) fms
eval' vals (Dsj fms) =  or $ map (eval vals) fms

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches
    -fwarn-monomorphism-restriction -fwarn-missing-signatures #-}
-- {-# HLINT ignore "Functor law" #-}

module FSemF () where

import Lib
import Data.List ( nub, sort )
import System.IO
import Data.Char
import FSynF ( Form(..) )

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

type Rel a = [(a, a)]

rSection :: Eq a => a -> Rel a -> [a]
rSection x r = [y | (z, y) <- r, z == x]

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = [(r0, s1) | (r0,r1) <- r, (s0,s1) <- s, r1 == s0]

-- reflexive transitive closure
rtc :: Ord a => [a] -> Rel a -> Rel a
rtc xs r = lfp (\s -> (sort . nub) $ s ++ (r @@ s)) i
  where
    i = [(x, x) | x <- xs]

-- least fixed point
lfp :: Eq a => (a -> a) -> a -> a
lfp f x
 | x == f x = x
 | otherwise = lfp f (f x)

data Class = Class String | OppClass String
  deriving (Eq, Ord)

instance Show Class where
  show (Class str) = str
  show (OppClass str) = "non-" ++ str

opp :: Class -> Class
opp (Class str) = OppClass str
opp (OppClass str) = Class str

type KB = [(Class, Class, Bool)]

data Statement = All Class Class | No Class Class | Some Class Class | SomeNot Class Class | AreAll Class Class | AreNo Class Class | AreAny Class Class | AnyNot Class Class | What Class
   deriving Eq
instance Show Statement where
  show (All as bs) ="All " ++ show as ++ " are " ++ show bs ++ "."
  show (No as bs) ="No " ++ show as ++ " are " ++ show bs ++ "."
  show (Some as bs) ="Some " ++ show as ++ " are " ++ show bs ++ "."
  show (SomeNot as bs) ="Some " ++ show as ++ " are not " ++ show bs ++ "."
  show (AreAll as bs) ="Are all " ++ show as ++ show bs ++ "?"
  show (AreNo as bs) ="Are no " ++ show as ++ show bs ++ "?"
  show (AreAny as bs) ="Are any " ++ show as ++ show bs ++ "?"
  show (AnyNot as bs) ="Are any " ++ show as ++ " not " ++ show bs ++ "?"
  show (What as) = "What about " ++ show as ++ "?"

isQuery :: Statement -> Bool
isQuery (AreAll _ _) = True
isQuery (AreNo _ _) = True
isQuery (AreAny _ _) = True
isQuery (AnyNot _ _) = True
isQuery (What _) = True
isQuery _ = True

neg :: Statement -> Statement
neg (AreAll as bs) = AnyNot as bs
neg (AreNo as bs) = AreAny as bs
neg (AreAny as bs) = AreNo as bs
neg (AnyNot as bs) = AreAll as bs
neg _ = undefined

subsetRel :: KB -> [(Class, Class)]
subsetRel kb = rtc classes $ trues ++ inversed
  where
    classes = domain kb
    trues = [(x,y) | (x,y,True) <- kb] -- A then B
    inversed = [(opp y, opp x) | (x,y,True) <- kb] -- not B then not A

domain :: KB -> [Class]
domain = nub . dom
  where
    dom [] = []
    dom ((xs,ys,_) : facts) = xs : opp xs : ys : opp ys : dom facts


supersets :: Class -> KB -> [Class]
supersets cl kb = rSection cl $ subsetRel kb

nsubsetRel :: KB -> [(Class, Class)]
nsubsetRel kb = s @@ r @@ s
  where
    r = nub $ [(x, y) | (x, y, False) <- kb]
      ++ [(opp y, opp x) | (x, y, False) <- kb]
      ++ [(Class xs, OppClass xs) | (Class xs, _, _) <- kb]
      ++ [(Class ys, OppClass ys) | (_, Class ys, _) <- kb]
      ++ [(Class ys, OppClass ys) | (_, OppClass ys, _) <- kb]
    s = [(y,x) | (x,y) <- subsetRel kb]

nsupersets :: Class -> KB -> [Class]
nsupersets cl kb = rSection cl $ nsubsetRel kb

derive :: KB -> Statement -> Bool
derive kb (AreAll as bs) = elem bs $ supersets as kb
derive kb (AreNo as bs) = elem (opp bs) $ supersets as kb
derive kb (AreAny as bs) = elem (opp bs) $ nsupersets as kb
derive kb (AnyNot as bs) = elem bs $ nsupersets as kb
derive _ _ = undefined

f2s :: (Class,Class,Bool) -> Statement
f2s (as, Class bs, True) = All as (Class bs)
f2s (as, OppClass bs, True) = No as (Class bs)
f2s (as, OppClass bs, False) = Some as (Class bs)
f2s (as, Class bs, False) = SomeNot as (Class bs)

tellAbout :: KB -> Class -> [Statement]
tellAbout kb as =
  [All as (Class bs) | (Class bs) <- supersets as kb, as /= (Class bs)]
  ++ [No as (Class bs) | (OppClass bs) <- supersets as kb, as /= (OppClass bs)]
  ++ [Some as (Class bs) | (OppClass bs) <- nsupersets as kb, as /= (OppClass bs)]
  ++ [SomeNot as (Class bs) | (Class bs) <- nsupersets as kb, as /= (Class bs), notElem (as, OppClass bs) (subsetRel kb)]

update' :: Statement -> KB -> Maybe (KB, Bool)
update' (All as bs) kb
  | elem bs (nsupersets as kb) = Nothing
  | elem bs (supersets as kb) = Just (kb, False)
  | otherwise = Just ((as, bs, True):kb, True)
update' (No as bs) kb
  | elem bs' (nsupersets as kb) = Nothing
  | elem bs' (supersets as kb) = Just (kb,False)
  | otherwise = Just (((as,bs',True):kb),True)
    where bs' = opp bs
update' (Some as bs) kb
  | elem bs' (supersets as kb) = Nothing
  | elem bs' (nsupersets as kb) = Just (kb,False)
  | otherwise = Just (((as,bs',False):kb),True)
    where
      bs' = opp bs
update' (SomeNot as bs) kb
  | elem bs (supersets as kb) = Nothing
  | elem bs (nsupersets as kb) = Just (kb,False)
  | otherwise = Just (((as,bs,False):kb),True)
update' _ kb = undefined

makeKB :: [Statement] -> Maybe KB
makeKB = makeKB' []
  where
    makeKB' kb [] = Just kb
    makeKB' kb (s:ss) = case update' s kb of
      Just (kb',_) -> makeKB' kb' ss
      Nothing -> Nothing

preprocess :: String -> [String]
preprocess = words . (map toLower) . (takeWhile (\x -> isAlpha x || isSpace x))

parse :: String -> Maybe Statement
parse = parse' . preprocess
  where
    parse' ["all",as,"are",bs] =Just (All (Class as) (Class bs))
    parse' ["no",as,"are",bs] =Just (No (Class as) (Class bs))
    parse' ["some",as,"are",bs] =Just (Some (Class as) (Class bs))
    parse' ["some",as,"are","not",bs] =Just (SomeNot (Class as) (Class bs))
    parse' ["are","all",as,bs] =Just (AreAll (Class as) (Class bs))
    parse' ["are","no",as,bs] =Just (AreNo (Class as) (Class bs))
    parse' ["are","any",as,bs] =Just (AreAny (Class as) (Class bs))
    parse' ["are","any",as,"not",bs] =Just (AnyNot (Class as) (Class bs))
    parse' ["what", "about", as] = Just (What (Class as))
    parse' ["how", "about", as] = Just (What (Class as))
    parse' _ = Nothing

process :: String -> KB
process txt = maybe [] id (mapM parse (lines txt) >>= makeKB)

mytxt :: String
mytxt = "all bears are mammals\n"++ "no owls are mammals\n"++ "some bears are stupids\n"++ "all men are humans\n"++ "no men are women\n"++ "all women are humans\n"++ "all humans are mammals\n"++ "some men are stupids\n"++ "some men are not stupids"

getKB :: FilePath -> IO KB
getKB p = do
  txt <- readFile p
  return $ process txt

writeKB :: FilePath -> KB -> IO ()
writeKB p kb = writeFile p (unlines (map (show .f2s) kb))


chat :: IO ()
chat = do
  -- kb <- getKB "kb.txt"
  let
    kb = process mytxt
  writeKB "kb.bak" kb
  putStrLn "Update or query the KB:"
  str <- getLine
  if str == ""
    then return ()
    else do
      case parse str of
        Nothing -> putStrLn "Wrong input.\n"
        Just (What as) ->
          let
            info = (tellAbout kb as, tellAbout kb (opp as))
          in
            case info of
              ([],[]) -> putStrLn "No info.\n"
              ([],negi) -> putStrLn (unlines (map show negi))
              (posi,negi) -> putStrLn (unlines (map show posi))
        Just stmt ->
          if isQuery stmt
          then
            if derive kb stmt
            then putStrLn "Yes.\n"
            else
              if derive kb (neg stmt)
              then putStrLn "No.\n"
              else putStrLn "I don???t know.\n"
          else
            case update' stmt kb of
              Just (kb',True) -> do
                writeKB "kb.txt" kb'
                putStrLn "OK.\n"
              Just (_,False) -> putStrLn"I knew that already.\n"
              Nothing -> putStrLn"Inconsistent with my info.\n"
  chat





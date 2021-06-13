module PrintableBdd where

import qualified Data.HasCacBDD as B
import Data.Bifunctor
import Data.List

data Bdd = Bdd
  { str :: String
  , tex :: String
  , bdd :: B.Bdd
  } deriving ( Eq, Read )

instance Show Bdd where
  show = str

directly :: (B.Bdd -> a) -> (Bdd -> a)
directly = flip (.) bdd

nullary :: String -> String -> B.Bdd -> Bdd
nullary = Bdd

unary :: String -> String -> (B.Bdd -> B.Bdd) -> (Bdd -> Bdd)
unary s t f b = Bdd (s ++ str b) (" " ++ t ++ " " ++ tex b) (f $ bdd b)

binary :: String -> String -> (B.Bdd -> B.Bdd -> B.Bdd) -> (Bdd -> Bdd -> Bdd)
binary s t f b1 b2 = Bdd
  (concat ["(", str b1, " ", s, " ", str b2, ")"])
  (concat ["(", tex b1, " ", s, " ", tex b2, ")"])
  (f (bdd b1) (bdd b2))

nary :: String -> String -> ([B.Bdd] -> B.Bdd) -> ([Bdd] -> Bdd)
nary s t f list = Bdd
  (s ++ "(" ++ intercalate ", " (map str list) ++ ")")
  (t ++ " (" ++ intercalate ", " (map tex list) ++ ")")
  (f $ map bdd list)

-- Nullary operators
varl :: (Int -> String) -> Int -> Bdd
varl l i = nullary (l i) (l i) (B.var i)

var :: Int -> Bdd
var = varl show

top :: Bdd
top = nullary "top" "\\top " B.top

bot :: Bdd
bot = nullary "bot" "\\bot " B.bot

-- Unary operators
neg :: Bdd -> Bdd
neg = unary "-" "\\neg" B.neg

-- Binary operators
con :: Bdd -> Bdd -> Bdd
con = binary "&&" "\\wedge" B.con

dis :: Bdd -> Bdd -> Bdd
dis = binary "||" "\\vee" B.dis

imp :: Bdd -> Bdd -> Bdd
imp = binary "->" "\\rightarrow" B.imp

equ :: Bdd -> Bdd -> Bdd
equ = binary "<->" "\\leftrightarrow" B.equ

-- N-ary operators
conSet :: [Bdd] -> Bdd
conSet = nary "CON" "\\bigwedge" B.conSet

disSet :: [Bdd] -> Bdd
disSet = nary "DIS" "\\bigvee" B.disSet

-- Special operators
substitl :: (Int -> String) -> Int -> Bdd -> Bdd -> Bdd
substitl l i s f = Bdd
  (concat ["[", l i, "\\", str s, "](", str f, ")"])
  (concat ["\\left[ ", l i, " \\setminus ", tex s, "\\right]\\left( ", tex f, " )"])
  (B.substit i (bdd s) (bdd f))

substit :: Int -> Bdd -> Bdd -> Bdd
substit = substitl show

substitlSimul :: (Int -> String) -> [(Int, Bdd)] -> Bdd -> Bdd
substitlSimul l pairs f = Bdd
  (concat ["[", intercalate ", " (map (\ (i, s) -> l i ++ "\\" ++ str s) pairs),  "](", str f, ")"])
  (concat ["\\left[", intercalate " , " (map (\ (i, s) -> " " ++ l i ++ "\\setminus " ++ str s ++ " ") pairs),  "\\right]\\left (", tex f, " )"])
  (B.substitSimul (map (second bdd) pairs) (bdd f))

substitSimul :: [(Int, Bdd)] -> Bdd -> Bdd
substitSimul = substitlSimul show

-- other
restrict :: Bdd -> (Int, Bool) -> Bdd
restrict b r = Bdd (str b) (tex b) (B.restrict (bdd b) r)

allVarsOf :: Bdd -> [Int]
allVarsOf = directly B.allVarsOf

evaluate :: Bdd -> B.Assignment -> Maybe Bool
evaluate = directly B.evaluate

evaluateFun :: Bdd -> (Int -> Bool) -> Bool
evaluateFun = directly B.evaluateFun
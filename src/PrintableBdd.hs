{-|
Module      : PrintableBdd
Description : A wrapper module implementing a Binary Decision Diagram system that can be 
              printed in either text or latex. This module internally uses the Data.HasCacBDD module. 
License     : BSD3
-}
module PrintableBdd
( Bdd(bdd, str, tex)
, VarLabeller
, varl
, var
, top
, bot
, neg
, con
, dis
, imp
, equ
, xor
, foralll
, forall
, existsl
, exists
, forallSetl
, forallSet
, existsSetl
, existsSet
, conSet
, disSet
, xorSet
, allVarsOf
, evaluate
, evaluateFun
) where

import qualified Data.HasCacBDD as B
import Data.Bifunctor ( Bifunctor(second) )
import Data.List ( intercalate )

import Data.Set (Set)

-- | Printable data wrapper for the Data.HasCacBDD.Bdd record
data Bdd = Bdd
  { str :: String
  , tex :: String
  , bdd :: B.Bdd
  } deriving ( Eq, Read )

instance Show Bdd where
  show = str

-- | The printable Bdd can label their variables, instead of using integers. A VarLabeller type is a function that encodes
--   a variable integer to its corresponding name.
type VarLabeller = (Int -> String)

-- | Encodes an arbitrary function that takes a single Data.HasCacBDD.Bdd argument to its printable counterpart
directly :: (B.Bdd -> a) -> (Bdd -> a)
directly = flip (.) bdd

-- | Encodes a nullary function of Data.HasCacBDD to its printable counterpart
nullary :: String -> String -> B.Bdd -> Bdd
nullary = Bdd

-- | Encodes a unary function (taking one Data.HasCacBDD.Bdd argument) of Data.HasCacBDD to its printable counterpart
unary :: String -> String -> (B.Bdd -> B.Bdd) -> (Bdd -> Bdd)
unary s t f b = Bdd (s ++ str b) (" " ++ t ++ " " ++ tex b) (f $ bdd b)

-- | Encodes a binary function (taking two Data.HasCacBDD.Bdd arguments) of Data.HasCacBDD to its printable counterpart
binary :: String -> String -> (B.Bdd -> B.Bdd -> B.Bdd) -> (Bdd -> Bdd -> Bdd)
binary s t f b1 b2 = Bdd
  (concat ["(", str b1, " ", s, " ", str b2, ")"])
  (unwords ["(", tex b1, t, tex b2, ")"])
  (f (bdd b1) (bdd b2))

-- | Encodes an n-ary function of Data.HasCacBDD to its printable counterpart
nary :: String -> String -> ([B.Bdd] -> B.Bdd) -> ([Bdd] -> Bdd)
nary s t f list = Bdd
  ("(" ++ intercalate (" " ++ s ++ " ") (map str list) ++ ")")
  (" (" ++ intercalate (" " ++ t ++ " ") (map tex list) ++ ") ")
  (f $ map bdd list)

-- | Encodes a binary quantification function of Data.HasCacBDD to its printable counterpart
quant :: VarLabeller -> VarLabeller -> String -> String -> (Int -> B.Bdd -> B.Bdd) -> (Int -> Bdd -> Bdd)
quant ls lt s t f v b = Bdd
  (concat [s, ls v, "(", str b, ")"])
  (unwords [t, lt v, "\\left( ", tex b, "\\right)"])
  (f v (bdd b))

-- | Encodes an n-ary binary quantification function of Data.HasCacBDD to its printable counterpart
quantSet :: VarLabeller -> VarLabeller -> String -> String -> ([Int] -> B.Bdd -> B.Bdd) -> ([Int] -> Bdd -> Bdd)
quantSet ls lt s t f v b = Bdd
  (concat [s, "{", intercalate ", " (map ls v), "}", "(", str b, ")"])
  (unwords [t, "\\left{", intercalate " , " (map lt v), "\\right}", "(", str b, ")"]) 
  (f v (bdd b))

{-
      Nullary operators
-}

-- | 
varl :: VarLabeller -> VarLabeller -> Int -> Bdd
varl ls lt i = nullary (ls i) (lt i) (B.var i)

var :: Int -> Bdd
var = varl show show

top :: Bdd
top = nullary "⊤" "\\top " B.top

bot :: Bdd
bot = nullary "⊥" "\\bot " B.bot

-- Unary operators
neg :: Bdd -> Bdd
neg = unary "¬" "\\neg" B.neg

-- Binary operators
con :: Bdd -> Bdd -> Bdd
con = binary "∧" "\\wedge" B.con

dis :: Bdd -> Bdd -> Bdd
dis = binary "∨" "\\vee" B.dis

imp :: Bdd -> Bdd -> Bdd
imp = binary "⟶" "\\rightarrow" B.imp

equ :: Bdd -> Bdd -> Bdd
equ = binary "⟷" "\\leftrightarrow" B.equ

xor :: Bdd -> Bdd -> Bdd
xor = binary "⊻" "\\oplus" B.xor

-- Quantification operators
foralll :: VarLabeller -> VarLabeller -> Int -> Bdd -> Bdd
foralll ls lt = quant ls lt "∀" "\\forall" B.forall

forall :: Int -> Bdd -> Bdd
forall = foralll show show

forallSetl :: VarLabeller -> VarLabeller -> [Int] -> Bdd -> Bdd
forallSetl ls lt = quantSet ls lt "∀" "\\forall" B.forallSet

forallSet :: [Int] -> Bdd -> Bdd
forallSet = forallSetl show show

existsl :: VarLabeller -> VarLabeller -> Int -> Bdd -> Bdd
existsl ls lt = quant ls lt "∃" "\\exists" B.exists

exists :: Int -> Bdd -> Bdd
exists = existsl show show

existsSetl :: VarLabeller -> VarLabeller -> [Int] -> Bdd -> Bdd
existsSetl ls lt = quantSet ls lt "∃" "\\exists" B.existsSet

existsSet :: [Int] -> Bdd -> Bdd
existsSet = existsSetl show show

-- N-ary operators
conSet :: [Bdd] -> Bdd
conSet = nary "∧" "\\wedge" B.conSet

disSet :: [Bdd] -> Bdd
disSet = nary "∨" "\\vee" B.disSet

xorSet :: [Bdd] -> Bdd
xorSet = nary "⊻" "\\oplus" B.xorSet

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

boolQuant :: Set Int -> Bdd -> Bdd
boolQuant v f = conSet $ concatMap (\var -> [substit var top f `con` substit var bot f]) v
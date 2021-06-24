{-|
Module      : PrintableBdd
Description : A wrapper module implementing a Binary Decision Diagram system that can be 
              printed in either text or latex. This module internally uses the Data.HasCacBDD module. 
Copyright   : (c) Jesper Kuiper, 2021
                  Leander van Boven, 2021
                  Ramon Meffert, 2021
License     : BSD3
-}

module PrintableBdd
( -- * BDD type
  Bdd
  -- ** BDD fields
, bdd
, str
, tex 
  -- * Other types
, Assignment
, VarLabeller
  -- * Creating new BDDs
, varl
, var
, top
, bot
  -- * BDD Operators
  -- ** Unary operator
, neg
  -- ** Binary operators
, con
, dis
, imp
, equ
, xor
  -- ** N-ary operators
, conSet
, disSet
, xorSet
  -- ** Quantification
, foralll
, forall
, existsl
, exists
  -- ** N-ary quantification
, forallSetl
, forallSet
, existsSetl
, existsSet
-- ** Substitution
, substitl
, substit
-- * Variable inspection
, allVarsOf
-- * Evaluation
, evaluate
, evaluateFun
) where

import qualified Data.HasCacBDD as B
import Data.Bifunctor ( Bifunctor(second) )
import Data.List ( intercalate )

import Data.Set (Set)

-- | Printable data wrapper for the `Data.HasCacBDD.Bdd` record.
data Bdd = Bdd
  { -- | The formula represented as a unicode string for direct printing.
    str :: String

    -- | The formula represented as a string, formatted in LaTeX.
  , tex :: String

    -- | The binary decision diagram, as a `Data.HasCacBDD.Bdd` type. 
  , bdd :: B.Bdd
  } deriving ( Eq, Read )

instance Show Bdd where
  show = str

-- | An assignment is a valuation for a set of variables, i.e. a valuation as to whether a variable is true or false. 
type Assignment = [(Int, Bool)]

-- | The printable BDD can label their variables, instead of using integers. A VarLabeller type is a function that encodes
--   a variable integer to its corresponding name.
type VarLabeller = (Int -> String)

-- | Encodes an arbitrary function that takes a single `Data.HasCacBDD.Bdd` argument to its printable counterpart.
directly :: (B.Bdd -> a) -> (Bdd -> a)
directly = flip (.) bdd

-- | Encodes a nullary function of Data.HasCacBDD to its printable counterpart.
nullary :: String -> String -> B.Bdd -> Bdd
nullary = Bdd

-- | Encodes a unary function (taking one `Data.HasCacBDD.Bdd` argument) of `Data.HasCacBDD` to its printable counterpart.
unary :: String -> String -> (B.Bdd -> B.Bdd) -> (Bdd -> Bdd)
unary s t f b = Bdd (s ++ str b) (" " ++ t ++ " " ++ tex b) (f $ bdd b)

-- | Encodes a binary function (taking two `Data.HasCacBDD.Bdd` arguments) of `Data.HasCacBDD` to its printable counterpart.
binary :: String -> String -> (B.Bdd -> B.Bdd -> B.Bdd) -> (Bdd -> Bdd -> Bdd)
binary s t f b1 b2 = Bdd
  (concat ["(", str b1, " ", s, " ", str b2, ")"])
  (unwords ["(", tex b1, t, tex b2, ")"])
  (f (bdd b1) (bdd b2))

-- | Encodes an n-ary function of `Data.HasCacBDD` to its printable counterpart.
nary :: String -> String -> ([B.Bdd] -> B.Bdd) -> ([Bdd] -> Bdd)
nary s t f list = Bdd
  ("(" ++ intercalate (" " ++ s ++ " ") (map str list) ++ ")")
  (" (" ++ intercalate (" " ++ t ++ " ") (map tex list) ++ ") ")
  (f $ map bdd list)

-- | Encodes a binary quantification function of Data.HasCacBDD to its printable counterpart.
quant :: VarLabeller -> VarLabeller -> String -> String -> (Int -> B.Bdd -> B.Bdd) -> (Int -> Bdd -> Bdd)
quant ls lt s t f v b = Bdd
  (concat [s, ls v, "(", str b, ")"])
  (unwords [t, lt v, "\\left( ", tex b, "\\right)"])
  (f v (bdd b))

-- | Encodes an n-ary binary quantification function of Data.HasCacBDD to its printable counterpart.
quantSet :: VarLabeller -> VarLabeller -> String -> String -> ([Int] -> B.Bdd -> B.Bdd) -> ([Int] -> Bdd -> Bdd)
quantSet ls lt s t f v b = Bdd
  (concat [s, "{", intercalate ", " (map ls v), "}", "(", str b, ")"])
  (unwords [t, "\\left{", intercalate " , " (map lt v), "\\right}", "(", str b, ")"]) 
  (f v (bdd b))

{-
      Nullary operators
-}

-- | Encode a labelled integer as a BDD variable. Note that the first labeller is for text, the second for LaTeX. The variable is indexed by any integer from 0 to 1.000.000.
-- 
-- >>> slabel n = "var" ++ (show n)
-- >>> tlabel n = "p_" ++ (show n)
-- >>> foo = varl slabel tlabel 0
-- >>> foo
-- var0
-- >>> tex foo
-- p_0
varl :: VarLabeller -> VarLabeller -> Int -> Bdd
varl ls lt i = nullary (ls i) (lt i) (B.var i)

-- | Encode an unlabelled integer as a BDD variable. The variable is indexed by any integer from 0 to 1.000.000.
var :: Int -> Bdd
var = varl show show

-- | True constant.
--
-- >>> top
-- ⊤
top :: Bdd
top = nullary "⊤" "\\top " B.top

-- | False constant.
--
-- >>> bot
-- ⊥
bot :: Bdd
bot = nullary "⊥" "\\bot " B.bot


{-
      Unary operators
-}

{- |
Negation.

>>> neg foo
¬(foo)
-}
neg :: Bdd -> Bdd
neg = unary "¬" "\\neg" B.neg


{-
      Binary operators
-}

-- | Conjunction. 
-- 
-- >>> foo `con` bar
-- (foo ∧ bar)
con :: Bdd -> Bdd -> Bdd
con = binary "∧" "\\wedge" B.con

-- | Disjunction.
--
-- >>> foo `dis` bar
-- (foo ∨ bar)
dis :: Bdd -> Bdd -> Bdd
dis = binary "∨" "\\vee" B.dis

-- | Logical implication.
--
-- >>> foo `imp` bar
-- (foo ⟶ bar)
imp :: Bdd -> Bdd -> Bdd
imp = binary "⟶" "\\rightarrow" B.imp

-- | Logical bi-implication or equivalence.
--
-- >>> foo `equ` bar
-- (foo ⟷ bar)
equ :: Bdd -> Bdd -> Bdd
equ = binary "⟷" "\\leftrightarrow" B.equ

-- | Exlusive or.
--
-- >>> foo `xor` bar 
-- (foo ⊻ bar)
xor :: Bdd -> Bdd -> Bdd
xor = binary "⊻" "\\oplus" B.xor


{-
      Quantification operators
-}

-- | Labelled binary universal quantification. Note that the first VarLabeller works for direct printing and the VarLabeller is for latex.
--
-- >>> slabel n = "var" ++ (show n)
-- >>> tlabel n = "p_" ++ (show n)
-- >>> foo = foralll slabel tlabel 0 $ varl slabel tlabel 0 `imp` varl slabel tlabel 1
-- >>> foo
-- ∀var0(var0 ⟶ var1)
-- >>> tex foo
-- \\forall p_0 (p_0 \\rightarrow p_1)
foralll :: VarLabeller -> VarLabeller -> Int -> Bdd -> Bdd
foralll ls lt = quant ls lt "∀" "\\forall" B.forall

-- | Unlabelled binary universal quantification. 
-- 
-- >>> forall 0 foo
-- ∀0(foo)
forall :: Int -> Bdd -> Bdd
forall = foralll show show

-- | Labelled binary existential quantification.
--
-- >>> slabel n = "var" ++ (show n)
-- >>> tlabel n = "p_" ++ (show n)
-- >>> foo = existsl slabel tlabel 0 $ (varl slabel tlabel 0) `imp` (varl slabel tlabel 1)
-- >>> foo
-- ∃var0(var0 ⟶ var1)
-- >>> tex foo
-- \\exists (p_0 \\rightarrow p_1) 
existsl :: VarLabeller -> VarLabeller -> Int -> Bdd -> Bdd
existsl ls lt = quant ls lt "∃" "\\exists" B.exists

-- | Unlabelled binary existential quantification
--
-- >>> exists 0 foo
-- ∃0(foo)
exists :: Int -> Bdd -> Bdd
exists = existsl show show

-- | Labelled big binary universal quanficition. 
--
-- >>> slabel n = "var" ++ (show n)
-- >>> tlabel n = "p_" ++ (show n)
-- >>> foo = forallSetl slabel tlabel [0, 1] $ (varl slabel tlabel 0) `imp` (varl slabel tlabel 1)
-- >>> foo
-- ∀{var0, var1}(var0 ⟶ var1)
-- >>> tex foo
-- \\forall \\left{ p_0, p_1 \\right} (p_0 \\rightarrow p_1)
forallSetl :: VarLabeller -> VarLabeller -> [Int] -> Bdd -> Bdd
forallSetl ls lt = quantSet ls lt "∀" "\\forall" B.forallSet

-- | Unlabelled big binary universal quantification. 
--
-- >>> forallSet [0,1,2] foo
-- ∀{0,1,2}(foo)
forallSet :: [Int] -> Bdd -> Bdd
forallSet = forallSetl show show

-- | Labelled big binary existential quantification.
--
-- >>> slabel n = "var" ++ (show n)
-- >>> tlabel n = "p_" ++ (show n)
-- >>> foo = existsSetl slabel tlabel [0, 1] $ (varl slabel tlabel 0) `imp` (varl slabel tlabel 1)
-- >>> foo
-- ∃{var0, var1}(var0 ⟶ var1)
-- >>> tex foo
-- \\exists \\left{ p_0, p_1 \\right} (p_0 \\rightarrow p_1)
existsSetl :: VarLabeller -> VarLabeller -> [Int] -> Bdd -> Bdd
existsSetl ls lt = quantSet ls lt "∃" "\\exists" B.existsSet

-- | Unlabelled big binary existential quantification.
--
-- >>> existsSet [0,1,2] foo
-- ∃{0,1,2}(foo)
existsSet :: [Int] -> Bdd -> Bdd
existsSet = existsSetl show show

-- N-ary operators

-- | Big (setwise) conjunction.
--
-- >>> conSet [foo, bar, baz]
-- (foo ∧ bar ∧ baz)
conSet :: [Bdd] -> Bdd
conSet = nary "∧" "\\wedge" B.conSet


-- | Big (setwise) disjunction.
--
-- >>> disSet [foo, bar, baz]
-- (foo ∨ bar ∨ baz)
disSet :: [Bdd] -> Bdd
disSet = nary "∨" "\\vee" B.disSet

-- | Big (setwise) exclusive disjunction.
--
-- >>> xorSet [foo, bar, baz]
-- (foo ⊻ bar ⊻ baz)
xorSet :: [Bdd] -> Bdd
xorSet = nary "⊻" "\\oplus" B.xorSet

-- Special operators

-- | Labelled substitution.
--
-- >>> label n = "var" ++ n
-- >>> subsitl label 0 top foo
-- [var0\⊤](foo)
substitl :: VarLabeller -> Int -> Bdd -> Bdd -> Bdd
substitl l i s f = Bdd
  (concat ["[", l i, "\\", str s, "](", str f, ")"])
  (concat ["\\left[ ", l i, " \\setminus ", tex s, "\\right]\\left( ", tex f, " )"])
  (B.substit i (bdd s) (bdd f))

-- | Unlabelled substitution.
--
-- >>> substit 0 top foo
-- [0\⊤]foo
substit :: Int -> Bdd -> Bdd -> Bdd
substit = substitl show

-- other

-- | Returns all variables (without labels) that occur within the given BDD.
--
-- >>> allVarsOf (var 0 `imp` (var 1 `dis` var 2))
-- [0,1,2]
allVarsOf :: Bdd -> [Int]
allVarsOf = directly B.allVarsOf

-- | Evaluate a BDD, given an assignment. Returns Nothing if the assignment doesn't contain all variables of the BDD.
--
-- >>> evaluate (var 0 `imp` var 1) [(0, True), (1, False)]
-- Just False 
-- >>> evaluate (var 0 `imp` var 1) [(0, True)]
-- Nothing
evaluate :: Bdd -> Assignment -> Maybe Bool
evaluate = directly B.evaluate

-- | Evaluates a BDD, given a total assignment function.
--
-- >>> evaluateFun (var 0 `imp` var 1) (\ x -> if x == 0 then True else False)
-- False
evaluateFun :: Bdd -> (Int -> Bool) -> Bool
evaluateFun = directly B.evaluateFun
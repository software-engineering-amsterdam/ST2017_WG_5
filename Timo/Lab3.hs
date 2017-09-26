module Lab3 where 

import Data.List
import Data.Char
import Test.QuickCheck

-- Assignment 3 (8 hours)

-- From the labs.----------------------------------------------
type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving (Eq,Ord)

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3 

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
form4 = Neg (Cnj [(Cnj [Dsj [p, q], Neg (Dsj [Neg p, q])]), p])

arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
---------------------------------------------------------

-- Apply de Morgan's distributive laws by using pattern matching. 
deMorgan :: Form -> Form
deMorgan (Prop f) = Prop f
deMorgan (Neg (Prop f)) = (Neg (Prop f))
deMorgan (Cnj fs) = Cnj (map deMorgan fs)
deMorgan (Dsj []) = Dsj []
deMorgan (Dsj [f]) = deMorgan f
deMorgan (Dsj (f:fs)) = deMorgan2 (deMorgan f) (deMorgan (Dsj fs))

-- Apply de Morgan's distributive laws by using pattern matching.
-- These are for repositioning a disjunction with a conjunction:
-- (A ^ B) v C --> (A v C) ^ (B v C)
deMorgan2 :: Form -> Form -> Form
deMorgan2 (Cnj []) _ = Cnj []
deMorgan2 (Cnj [f]) g = deMorgan2 f g
deMorgan2 (Cnj (f:fs)) g = Cnj [deMorgan2 f g, deMorgan2 (Cnj fs) g]
deMorgan2 _ (Cnj []) = Cnj []
deMorgan2 f (Cnj [g]) = deMorgan2 f g
deMorgan2 f (Cnj (g:gs)) = Cnj [deMorgan2 f g, deMorgan2 f (Cnj gs)]
deMorgan2 f g = (Dsj [f,g])

--getDoubleList :: Form -> [Form] -> [Form]
--getDoubleList (Dsj []) list = list
--getDoubleList (Dsj [f]) list = list ++ [f]
--getDoubleList (Dsj (f:fs)) list = getDoubleList (Cnj fs) (list ++ [f])
--getDoubleList (Cnj []) list = list
--getDoubleList (Cnj [f]) list = list ++ [f] 
--getDoubleList (Cnj (f:fs)) list = getDoubleList (Dsj fs) (list ++ [f])
--getDoubleList _ list = list
--getDoubleList _ list = list

--createNotDoubleForm :: [Form] -> Form
--createNotDoubleForm [f] frm = Cnj [frm, f] 
--createNotDoubleForm (f:fs) frm = createNotDoubleForm fs (Cnj [frm, f]) 

--removeDouble :: Form -> [Form]
--removeDouble frm = (nub (getDoubleList frm []))

-- This method will converse formulas of propositional logic into cnf form.
cnf ::  Form -> Form
cnf x = (deMorgan (nnf (arrowfree x)))

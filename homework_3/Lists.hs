module Lists where

import Prelude
import Terms
import Subst
import Beta
import YCombinator
import Booleans
import Church
import Factorial
import Fibonacci
import Pairs

-- your code here...

--cons:=λx.λl.pair false (pair x l)
cons :: Term -> Term -> Term
cons x l = App (App pair Booleans.false) (App (App pair x) l)

--nil:=λl.l
nil :: Term
nil = Lambda "x" (Var "x")

--hd:=λl.first (second l)
hd :: Term -> Term
hd t = first (second t)

--tl:=λl.second (second l)
tl :: Term -> Term
tl t = second (second t)

--isNull:=first
isNull :: Term -> Term
isNull l = (Pairs.first l)

--length':=λƒ.λl.ite (isNull l) (zero) (addition (one) (ƒ (tl l)))
length' :: Term
length' = Lambda "f" (Lambda "l" (ite (isNull (Var "l")) (zero) (addition (one) (App (Var "f") (tl (Var "l"))))))

--apply yCombinator to length'
length :: Term -> Term
length l = App(App(yCombinator)(length'))(l)

--append':=λƒ.λa.λb ite (isNull a) (b) (cons (hd a) (ƒ (tl a) b))
append' :: Term
append' = Lambda "f" (Lambda "a" (Lambda "b" (ite (isNull (Var "a")) (Var "b") (cons (hd (Var "a")) (App (App (Var "f") (tl (Var "a"))) (Var "b"))))))

--apply yCombinator to append'
append :: Term -> Term -> Term
append a b = App(App(App(yCombinator)(append'))(a))(b)

--reverse':=λƒ.λl.λnil.ite (isNull l) (nil) (ƒ (tl l) (cons (hd l) (nil)))
reverse' :: Term
reverse' = Lambda "f" (Lambda "l" (Lambda "nil" (ite (isNull (Var "l")) (Var "nil") (App (App (Var "f") (tl (Var "l"))) (cons (hd (Var "l")) (Var "nil"))))))

--apply yCombinator to reverse'
reverse :: Term -> Term
reverse l = App(App(App(yCombinator)(reverse'))(l))(nil)
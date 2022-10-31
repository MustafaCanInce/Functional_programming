{-# LANGUAGE GADTs #-}  -- do not remove or modify this line
{-# LANGUAGE ScopedTypeVariables #-}
import Prelude
import Data.List
import System.IO


{- Positive Integers -}

data Pos where
    XI :: Pos -> Pos
    XO :: Pos -> Pos
    XH :: Pos 

--Your code here...

printPos :: Pos -> String
printPos zeroth_case =
  case zeroth_case of 
    XH -> "XH"
    XI k -> "XI (" ++ printPos k ++ ")"
    XO k -> "XO (" ++ printPos k ++ ")"

pos2Int :: Pos -> Int
pos2Int third_case =
  case  third_case of
      XH -> 1
      XI s -> 1+ 2 * pos2Int s
      XO s -> 2 * pos2Int s


instance Show Pos where show p = show (pos2Int p)

posEq :: Pos -> Pos -> Bool 
posEq binary_eq_1 binary_eq_2 = pos2Int(binary_eq_1) == pos2Int(binary_eq_2)

instance Eq Pos where
  p == q = posEq p q


{-posLeq :: Pos -> Pos -> Bool
posLeq 
instance Ord Pos where
p <= q = posLeq p q
-}

int2pos :: Int -> Pos
int2pos first_case =
    case first_case of
      1 -> XH
      first_case -> 
        case mod first_case 2 of
            1 -> XI(int2pos(div first_case 2))
            0 -> XO(int2pos(div first_case 2))
          



posAdd :: Pos -> Pos -> Pos -- addition

posAdd binary_add_1 binary_add_2 = 
  int2pos(pos2Int(binary_add_1) + pos2Int(binary_add_2))

posSubtr :: Pos -> Pos -> Pos -- subtraction

posSubtr  binary_subt_1 binary_subt_2 = 
  int2pos(pos2Int(binary_subt_1) - pos2Int(binary_subt_2))

posMult :: Pos -> Pos -> Pos -- multiplication

posMult  binary_mult_1 binary_mult_2 = 
  int2pos(pos2Int(binary_mult_1) * pos2Int(binary_mult_2))
    
{-posAbs :: Pos -> Pos -- absolute value calculation
posAbs x =
  case x of
  pos2Int(x) >= 1 : return x
  pos2Int(x) <= 1 : return int2pos(-pos2Int(x))-}


--posSignum :: Pos -> Pos -- sign calculation
integer2pos :: Integer -> Pos
integer2pos fourth_case =
    case fourth_case of
      1 -> XH
      fourth_case -> 
        case mod fourth_case 2 of
            1 -> XI(integer2pos(div fourth_case 2))
            0 -> XO(integer2pos(div fourth_case 2))

posFromInteger :: Integer -> Pos -- conversion from Integer
posFromInteger int1 = integer2pos(int1)


-- ######################################################

{- Rational Numbers -}

data Rat where
    Frac :: Int -> Pos -> Rat

-- your code here...


{-
in which a single constructor named Frac formalizing fractions is employed. Observe that the first argument of
Frac is an integer encoding the numerator while the second argument is a positive integer implementing the
denominator of an arbitrarily given fraction. E.g., Frac (-2) (XI (XO (XI XH))) connotes the number
− 2/13 ∈ Q while the number 5/2 ∈ Q is represented by Frac 5 (XO XH).
-}

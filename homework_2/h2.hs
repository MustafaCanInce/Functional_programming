

{-# LANGUAGE GADTs #-}

module DFA where

import Prelude
import Data.Set
import Data.Set.Internal

type State = Int

data DFA =
    DFA
    {
        dfa_state    :: Set State,              -- Q
        dfa_alphabet :: Set Char,               -- 
        dfa_sigma    :: State -> Char -> State, -- δ: Q →  → Q
        dfa_start    :: State,                  -- s ∈ Q
        dfa_finals   :: Set State               -- F ⊆ Q

    }

-- your code here...



-- I describe the Dfa Function of the first DFA in the pdf.

-- Uses this Dfa structure for the input of dfa_multistep and dfa_acceptance.
dfa1 = DFA (fromList [0..5]) (fromList ['a','b']) dfa_func 0 (fromList [5])

dfa_func :: State-> Char -> State
dfa_func 0 'a' = 1
dfa_func 0 'b' = 0
dfa_func 1 'a' = 2
dfa_func 1 'b' = 0
dfa_func 2 'a' = 2
dfa_func 2 'b' = 3
dfa_func 3 'a' = 4
dfa_func 3 'b' = 0
dfa_func 4 'a' = 2
dfa_func 4 'b' = 5
dfa_func 5 'a' = 5
dfa_func 5 'b' = 5
dfa_func _  _  = error "State isn't defined"


-- I describe the Dfa function of the second DFA in the pdf.

-- Uses this Dfa structure for the input of dfa_multistep and dfa_acceptance.
dfa2 = DFA (fromList [0..13]) (fromList ['a']) dfa_func2 0 (fromList [0,2,4,6,7,8,10,12])

dfa_func2 :: State -> Char -> State
dfa_func2  0 'a' = 1
dfa_func2  1 'a' = 2
dfa_func2  2 'a' = 3
dfa_func2  3 'a' = 4
dfa_func2  4 'a' = 5
dfa_func2  5 'a' = 6
dfa_func2  6 'a' = 7
dfa_func2  7 'a' = 8
dfa_func2  8 'a' = 9
dfa_func2  9 'a' = 10
dfa_func2 10 'a' = 11
dfa_func2 11 'a' = 12
dfa_func2 12 'a' = 13
dfa_func2 13 'a' = 0
dfa_func2 _  _  = error "State isn't defined."




-- I describe the dfa_multiStep function takes DFA, State, String and returns state based on inputted String.
dfa_multiStep :: DFA -> State -> String -> State
dfa_multiStep dfa state [] = state
dfa_multiStep dfa state (x:xs) = dfa_multiStep dfa (dfa_sigma dfa state x) xs



-- I describe the dfa_acceptance function takes Dfa, String and returns True if the string is accepted by the DFA. and returns False if the string is not accepted by the DFA.
dfa_acceptance :: DFA -> String -> Bool
dfa_acceptance dfa string = 
    member (dfa_multiStep dfa (dfa_start dfa) string) (dfa_finals dfa)
     

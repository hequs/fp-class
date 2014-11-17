import Data.List

{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

	
-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect (a, s, is, td, aSt) = undefined


-- Напишите функцию, определяющую, допускает ли НКА заданное слово 
accept :: NFA -> String -> Bool
accept (a, s, is, td, aSt) word = length (intersect (foldl makeStep [is] word) aSt) > 0
	where
		makeStep states c = states >>= (\state -> td state c)
		

-- Постройте ещё как минимум три примера НКА
-- Распознает цепочки вида 0101...
nfa1 :: NFA
nfa1 = (['0','1'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 '0' = [2]
    tf 1 '1' = []
    tf 2 '0' = []
    tf 2 '1' = [3]
    tf 3 '0' = [2]
    tf 3 '1' = []

-- Распознает цепочки с предпоследним 0
nfa2 :: NFA
nfa2 = (['0','1'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 '0' = [1, 2]
    tf 1 '1' = [1]
    tf 2 '0' = [3]
    tf 2 '1' = [3]
    tf 3 '0' = []
    tf 3 '1' = []

-- Распознает цепочки вида 0000...
nfa3 :: NFA
nfa3 = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1, 2]
    tf 1 '1' = [1]
    tf 2 '0' = []
    tf 2 '1' = []

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify nfas words = nfas >>= (\nfa -> [(nfa, filter (accept nfa) words)])
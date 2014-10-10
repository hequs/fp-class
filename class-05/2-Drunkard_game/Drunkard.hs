{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Hearts | Diamonds | Clubs
	deriving (Show, Eq)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Show, Eq, Ord)

data Card = Card Value Suit
	deriving (Show, Eq)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2) = s1 == s2

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 _) `beats` (Card v2 _) = compare v1 v2

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round cards = game_round' (cards, [])
	where
		game_round' (((c1:cards1), (c2:cards2)), stack)
			| c1 `beats` c2 == GT = (cards1 ++ (c1:c2:stack), cards2)
			| c1 `beats` c2 == LT = (cards1, cards2 ++ (c1:c2:stack))
			| otherwise = game_round' ((cards1, cards2), (c1:c2:stack))

game_round_test1 = game_round ([Card Two Diamonds, Card Three Hearts], [Card King Hearts, Card Queen Clubs]) 
	== ([Card Three Hearts], [Card Queen Clubs, Card Two Diamonds, Card King Hearts])
	
game_round_test2 = game_round ([Card Two Diamonds, Card Three Hearts], [Card Two Hearts, Card Queen Clubs]) 
	== ([], [Card Three Hearts, Card Queen Clubs, Card Two Diamonds, Card Two Hearts])
	
{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
	deriving (Show, Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game cards = game' (cards, 0)
	where
		game' (([], _), count) = (Second, count)
		game' ((_, []), count) = (First, count)
		game' (cards, count) = game' (game_round cards, count + 1)

cards_set1 = [Card Two Diamonds, Card Four Diamonds, Card Four Spades, Card Three Clubs, Card Two Diamonds]
cards_set2 = [Card King Hearts, Card Ace Clubs, Card Jack Spades, Card Ace Clubs, Card King Spades]

game_test1 = game (cards_set1, cards_set2) == (Second,5)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}

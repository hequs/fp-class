{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}

import System.Environment

main = do
	[action, file] <- getArgs
	case action of 
		"1" -> genPointFile file

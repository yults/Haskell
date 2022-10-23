module Lecture1 (main) where
import Numeric.Natural
-- Haskell ---------------------------------
--   GHC - компилятор
--   stack - чтобы сбилдить 
--   cabal - мета информация 
--   snapshot - набор стабильных версий всех библиотек (избавление от dependency hell)
--   статический, иммутабельный (не именяемые переменные)
--   purity, non-null, ленивые вычисления
--   чистые функции (зависят только от своих аргументов, не изменяют глобальное
--   состояние, отрабатывают одинаково на одних и тех же переменных)
--   Исходный файл - модуль. Имя модуля = полный путь файла. Из модулей можно собрать package
--   Создать проект stack new newProject
--   stack build
--   stack run
--   stack ghci

-- GHCI ------------------------------------
--   типа repl, интерактивная среда
--   :q . *Empty> :q Leaving GHCi -- выход
--   Загрузить из файла в ghci 
--   :l Lecture1.hs
--   :t - узнать тип, в ghci чтобы видеть всегла :set +t

-- Установка -------------------------------
--   |Идем на сайт ghcup - там скачиваем. Если возникает проблема с curl, на сайте 
--   вкладка Troubleshooting. Отключить антивирус перед скачкой, могут возникнуть
--   проблемы с сертификатом. Далее когда будет успешно, идем в VS code, находим
--   расширение которое зависит от ghcup (называется просто - Haskell). Там есть инструкция,
--   сделать как там. Вкратце - зайти в переменные среды, занести в Path stack, cabal

import Lib

-- Основы -----------------------------------
--   2 + 2 /= 5 не равно
--   div 7 3 + 1 = 3 у функции высший приоритет
--   Prefix функции и операторы в скобках :
--   min 1 2, (+) 3 7
--   Infix операторы и функции в `` :
--   1 + 2, 7 `mod` 3
--   "Hello " ++ "world!" конкатенация строк

-- Обьявление функций -----------------------
addMul :: Int -> Int -> Int -> Int -- 3 аргумента и результат
addMul x y z = x + y * z
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
main :: IO ()
main = putStrLn (greet "Tus") -- console.log
--let max5 x = max 5 x 
--let max5' = max 5 -- частичное примененеи функции - эквиваленто тому что выше

-- Собственные операторы --------------------
--   infix|infixr|infixl приоритет (0; 10] название оператора
infixr 1 ==> -- обьявление импликации (правоассоциативная приоритет 1 имя ==>)
(==>) ::   Bool -> Bool -> Bool 
a ==> b = not a || b
--   правоассоциативная значит скобки расставляются справа налево т.е с конца
--   space приоритет 10, || 2, && 3
--   infixl : a * b * C * d === ((a * b) * c) * d
--   infixr : a * b * C * d === a * (b * (c * d))
--   infix : a * b * C * d === error

-- Let VS Where------------------------------ https://wiki.haskell.org/Let_vs._Where
--  В общем то одно и тоже но let сначала объявление, применение дальше
--  а where это сначала применение потом объявление
-- Let -----------
sumSquare :: Int -> Int -> Int 
sumSquare x y = x^2 + y^2
--   with let
sumSquareLet :: Int -> Int -> Int 
sumSquareLet x y =  let x2 = x ^ 2 
                        y2 = y ^ 2
                    in x2 + y2 
-- Where -----------
sumSquareWhere :: Double -> Double -> Double
sumSquareWhere a b = a2 + b2
  where  -- details of implementation are inside 'where'
    square x = x ^ 2
    a2       = square a
    b2       = square b
-- If --------------------------------
factorial :: Integer -> Integer  --длинная арифметика 
factorial n = if n <= 1 
              then 1 -- then обязателен 
              else n * factorial (n - 1)  
-- Guards ----------------------------
-- длинные if else типа case
collatzSum :: Natural -> Natural
collatzSum n
  | n == 1    = 1
  | even n    = n + collatzSum (n `div` 2)
  | otherwise = n + collatzSum (3 * n + 1)
-- Case ------------------------------
getFont :: Int -> String
getFont n = case n of
                0 -> "PLAIN"
                1 -> "BOLD"
                2 -> "ITALIC"
                _ -> "UNKNOWN"

caseOperation 
  :: Char -> Int -> Int -> Int
caseOperation op x y = 
    case op of
        '+' -> x + y
        '-' -> x - y
        _   -> 0   -- _ should be 
                    -- under ' and 
                    -- not under -
--  caseOperation '+' 1 2 -- 3
-- Типы ------------------------------------
--  List ------ 
--   [1 + 2, 3 + 4, 5 * 6]
--   let list = [2, 1, 3]
--   [5, 10] ++ list -- [5, 10, 2, 1, 3]
--   10 : list -- добавить 10 в начало  листа
--   reverse list -- [3, 1, 2]
emptyList :: [Int] -- обЪявлние const
emptyList = []
singletonList :: [Int]
singletonList = 1 : emptyList
listExample :: [Int]
listExample = [2, 1, 3]
listExample2 = 5:10:listExample -- [5, 10, 2, 1, 3]
twoLists = singletonList ++ listExample -- конкатенация [1, 2, 1, 3]
treeLists = singletonList ++ [7] ++ listExample -- [1, 7, 1, 2, 1, 3]
string :: [Char]
string = "str" -- ['s', 't', 'r']
--   "" == [] True
--  Ranges -----
--   [0 .. 5] -- [0, 1, 2, 3, 4, 5]
--   [1, 3 .. 7] -- [1, 3, 5, 7]
--   [0..] -- [0, 1, 2, 3 ...] infinite list
--   [0, 2 ..] -- [0, 2, 4, 6 ...] all even numbers
--   [5, 4, .. 1] --[5, 4, 3, 2, 1] - обратная последовательность
--   [5 .. 1] -- [] ошибка, нужен явный шаг
--   тут могут быть любые упорядочевыемые типы
--  Lists Functions---
--   Часть 1
--   let l = [2, 1, 3]
--   head l -- 2
--   tail l -- [1, 3]  
--   last l -- 3
--   init l -- [2, 1]
--   Типа иллюстация
--   head taaaaail
--   iniiiiit last
--   конец иллюстрации
--   все что выше лучше не использовать, потому что падает на пустом
--   Часть 2 Всяко разное
--   drop 2 [2, 1, 3] - выкинкуть последние элементы списка (2 штуки тут) -- [1, 3]
--   take 1 [2, 1, 3] - берет первые элементы списка (2 штуки тут) -- [2, 1]
--   replicate 3 [1..5]  - повторить -- [[1..5], [1..5], [1..5]]
--   zip [1, 2, 3] "abc" - создать список пар -- [(1,'a'),(2,'b'),(3,'c')]
--   unzip [(5, True), (10, False)] -- ([5,10],[True,False])
--   words "Hello,    Haskell \t\n\n   world!" - типа сплит скипWS -- ["Hello,","Haskell","world!"]
--   unwords ["Hello,","Haskell","world!"] -- "Hello, Haskell world!"
-- 

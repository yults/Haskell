module Lecture2 (main) where
import Numeric.Natural ( Natural )
import Lib ()
main :: IO()
main = putStrLn "Lecture2"
-- HOF - Higher order functions-------------------------
--   можем напрямую передавать функцию как аргумент другой функции
--   присвать функцию константе
--   возвращать функцию из функции
inc, dec :: Int -> Int
inc x = x + 1 --just function
dec x = x - 1
changeTwiceBy :: (Int -> Int) -> Int -> Int  --HOF принимает функцию и число делает из этого число
changeTwiceBy operation value = operation (operation value) -- changeTwiceBy inc 5 -- 7
--   changeTwiceBy (\x -> x + 1) 7 -- 9 - вместо функции можно передавать лямбду
tripleApply :: (Int -> Int -> Int) -> Int -> Int 
tripleApply f x = (x `f` x) `f` (x `f` x)
--   tripleApply f x = f (f x x) (f x x) - same as below
--   tripleApply (*) 3 -- 81
--   tripleApply (\x y -> x * y) 3 -- 81
-- Polymorfizm - начнем с параметрически полиморфныt
id :: a -> a  -- не важно какой тип - произвольный и пишем с маленькой буквы
id x = x
--  fst :: (a, b) -> a -кортежи 
--  snd :: (a, b) -> b
emptyList :: [a]
emptyList = []
repeatTree :: a -> [a]
repeatTree x = [x, x, x] 
--  repeatTree 7 - [7,7,7] ; repeatTree 'x' - "xxx"; repatTree True - [True, True, True]
--HOF Polimorfizm 
-- комбинаторы стандартной библиотеки
--  map :: (a -> b) -> [a] -> [b]
--  filter :: (a -> Bool) -> [a] -> [a]
--  foldr1 :: (a -> a -> a) -> [a] -> a 
--  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--   Примеры
--   map negate [2, 1, 3] -- [-2,-1,-3]
--   filter odd [1, 2, 3, 4, 5] -- [1,3,5]
--   foldr1 (+) [1, 2, 4] -- 7 (sum 1 2 3)
--   zipWith max [1..5] [5,4..1] -- [5,4,3,4,5] -- пара 1 5 выбираем 5 пара 2 4 выбираем 4
uncurry :: (a -> b -> c) -> (a, b) -> c --достаем first second из кортежа и проделывам над ними операцию
uncurry f p = f (fst p) (snd p)  --uncurry (+) (3, 4) --7
--   curry fst 3 4 -- 3
--  Функции в хаскеле имеют только одну переменную и один резутат
--   map (5+) [1..5]  -- [6, 7, 8, 9, 10]
--   map (+5) [1..5]  -- [6, 7, 8, 9, 10]
--   map (5-) [1..5]  -- [4,3,2,1,0]
--   map (-5) [1..5] -- error
--   subtract 5 3  -- -2
--   map (subtract 5) [1..5] -- [-4, -3, -2, -1, 0]
flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b
show2 :: Int -> Int -> String
show2 x y = show x ++ " and " ++ show y
-- Pattern mathcing - сопоставление с образцом
fact :: Integer -> Integer  -- факториал с паттерн матчингом
fact 0 = 1 -- отдельное тело для случая, сопоставление сверху вниз 
fact n = n * fact (n - 1) -- вместо case 
sumList3 :: [Int] -> Int -- для списка из 3 элементов сумма элементов, для других списков 0
sumList3 [x, y, z] = x + y + z
sumList3 _         = 0
--   map :: (a -> b) -> [a] -> [b] -- свой map
--   map _    []  = []
--   map f (x:xs) = f x : map f xs -- [2, 1, 3] == 2 : 1 : 3 : []
--   dropWhile :: (a -> Bool) -> [a] -> [a]  -- свой dropwhile
--   dropWhile _      []  = []
--   dropWhile p l@(x:xs) = if p x then dropWhile p xs else l  -- тут локальный элиас, не поняла
-- Расширения -----------------------------------
--  Как подрубить
--   in ghci
--   ghci> :set -X<NameOfPragma>
--   in MyModule.hs
--   {-- -# LANGUAGE NameOfPragma #-}
--   некоторые фичи по дефолту не стоят надо чета подрубить - language pragms
--   -XTupleSections - чуть меньше кода вметсо лямбды
--   -XLambdaCase - вместо патерн матчинга делаем лямбду с case 
--   -XViewPatterns - применить функцию и потом попатернматчить результат можно без case of
--Применение функций
-- space это функция
-- оператор чтобы избежать скобочек $ - минимальный приоритет правоассоциативынй
-- доллар это эквивалент пробелу но с большой разницой в приоритетах
-- Пример
-- div 7 (3 + 1) эквивалентно div 7 $ 3 + 1
foo, bar :: [Int] -> Int
foo list = length (filter odd (map (div 2) (filter even (map (div 7) list))))
bar list = length $ filter odd $ map (div 2) $ filter even $ map (div 7) list
-- композиция
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)  -- same as (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x) -- сначала применяет g к x потом f ко всему этому пример
-- x = negate (x + 1) = negate $ x + 1 = (negate . (+1)) x =
--  = negate . (+1) $ x но еще круче
-- incNegate :: Int -> Int - в бесточечном стиле
-- incNegate  = negate . (+1) - можно отбросить x но у меня не вышло
-- Списки сахар
-- [x | x <- [1..10], even x] = filter even [1..10]
-- [if even x then "!" else "?" | x <- [1..5]] --["?","!","?","!","?"]
-- [ x * y | x <- [1, 3, 5], y <- [2, 4, 6], x * y >= 10]  -- [12,18,10,20,30]
-- ghci> [13 | even 13]  -- conditionally create singleton list
-- []
--  ghci> [14 | even 14]
--  [14]
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) 
    = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]
-- Ленивые вычисления ----------------------------------
-- square x = x*x square (1+2) => (1+2)*(1+2) -- плохо, вычисляем 2 раза одно и ттже
-- Но это неправда там в графе один айдишник им присвоится и в графе просто возьмем
-- Нормальная форма  NF - если не имеет невычесленных подвыражений
-- Слабая головная нормальная форма WHNF - если является либо конструктором либо лямбдой
-- конструктор это разделенный список на голову и хвост грубо говоря
-- Когда происходит паттерн матчинг происходит вычисление выражения до WHNF
--  Ленивая стратегия всегда не хуже по времени чем строгая(энергичная)
--  Невычисленное выражение обычно больше по памяти поэтому неэффективно по памяти
--  Из-за ленивого вычисления тащим с собой большое невычисленное выражение
--  называется space leak
--  Форсирование вычислений - там пропаттернматчить
--  Приколы с бесконечным списком основанные на ленивой модели
--  take 5 [1..] -- [1,2,3,4,5]
--  zip [0..] "abacaba" -- [(0,'a'), (1,'b'), (2,'a'), (3,'c'), (4,'a'), (5,'b'), (6,'a')]
-- генерируем последовательности с помощью ленивых вычислений
primes :: [Int]
primes = filterPrime [2..] 
  where 
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
-- take 15 primes  -- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs) -- бесконечный рекурсивный список
fib :: Int -> Int
fib n = fibs !! n
-- работает за линию ghci> take 10 fibs --[0,1,1,2,3,5,8,13,21,34]
-- fib 50 -- 12586269025

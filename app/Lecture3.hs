module Lecture3 (main) where
    
import Numeric.Natural ( Natural )
import Lib ()
main :: IO()
main = putStrLn "About types"
-- Кастомные типы ----------------------
-- Type aliases - синоним типа
type User = (Int, String, String)
-- Алгебраические типы данных ADT - составной тип, способом комбинирования типов
-- произведение типов produсt types - PT = T_1 * T_2 .. * T_N типа коньюнкция
-- все  должны быть населены
-- тип населен если существует хотя бы один эл этого типа
-- сумма типов -- когда населен только один из них
-- ST = T_1 + T_2 .. + T_N
-- ADT = PrimitiveType |ADT+ADT| ADT*ADT
-- a) enums
data TrafficLight = Red | Yellow | Green | Blue
-- pattern matching with types
lightName :: TrafficLight -> String
lightName Red    = "red"
lightName Yellow = "yellow"
lightName Green  = "green"
lightName Blue   = "magenta"
--ghci> map lightName [Yellow, Red, Blue, Yellow] --["yellow","red","magenta","yellow"]
-- b) stucture 
data User2 = MkUser Int String String -- MkUser - конструктор данных
getUid :: User2 -> Int
getUid (MkUser uid _ _) = uid    -- pattern should be in ()
getName :: User2 -> String
getName (MkUser _ name _) = name
-- ghci> :t MkUser
-- MkUser :: Int -> String -> String -> User
-- Параметризованные
data Point2D a = Point2D a a  -- constructor name can be the same as type name
doublePoint :: Point2D a -> Point2D (a, a)
doublePoint (Point2D x y) = Point2D (x, y) (x, y) 
-- ghci> pointToList (Point2D 5 10)  -- [5, 10] 
-- ghci> doublePoint (Point2D 'a' 'b') -- Point2D ('a', 'b') ('a', 'b') 

-- сумма типов
data IntResult = Success Int 
                | Failure String
-- применение
safeDiv :: Int -> Int -> IntResult
safeDiv _ 0 = Failure "division by zero"
safeDiv x y = Success $ x `div` y
showResult :: IntResult -> String
showResult (Success n) = "Result: " ++ show n
showResult (Failure e) = "Error:  " ++ e
--ghci> showResult $ safeDiv 7 2
--"Result: 3"
--ghci> showResult $ safeDiv 7 0
--"Error: division by zero"
-- param sum
data Vector a = Vector2D a a | Vector3D a a a -- вектор из 2ух парам или 3
packVector :: Vector a -> [a]
packVector (Vector2D x y)   = [x, y]
packVector (Vector3D x y z) = [x, y, z]
-- Maybe -- либо успех (Just) либо что то пошло не так напимер нул (Nothing)   
--data Maybe a = Nothing | Just a  -- implemented in Prelude
-- Just - унарный конструктор
-- Nothing - нулярный конструктор - что то пошло не так
maybeSecond :: [a] -> Maybe a -- хотим вернуть список если его длина больше 2
maybeSecond (_:x:_) = Just x --списки длины 2+
maybeSecond _       = Nothing --списки длины 1 и 0
-- A good way to avoid null poiner bugs!
-- Either - типа Maybe но понятно что пошло не так
--data Either a b = Left a | Right b  -- implemented in Prelude
-- Успех это Right и туда можно запистаь резульатт типа Just
-- поражение это Left можно выкинуть сообщение об ошибке типа Nothing но подробно
-- eitherSecond хотим вернуть список если его длина больше 2
eitherSecond :: [a] -> Either String a -- если left вернет строку если right вернет a
eitherSecond []      = Left "list is empty"
eitherSecond [_]     = Left "list has only single element"
eitherSecond (_:x:_) = Right x
-- рекурсивный тип 
data List a = Nil | Cons a (List a) 
-- список это пустой список (нулярный) + конструктор принимает значения типа a
-- и значение типа лист а (рекурсия) т е пара из головы и хвоста
myMap :: (a -> b) -> List a -> List b
myMap _        Nil  = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs) 
-- record syntax
data User4 = User4
    { uid      :: Int
    , login    :: String
    , password :: String 
    }-- короче это геттеры
-- это короткая запись следующего
data User11 = User11 Int String String
uid11 :: User11 -> Int
uid11 (User11 i _ _) = i
login11 :: User11 -> String
login11 (User11 _ l _) = l
password11 :: User11 -> String
password11 (User11 _ _ p) = p
-- пример использования
ivan :: User4
ivan = User4 { login    = "Ivan"
            , password = "123" 
            , uid      = 1
            } --можно в рандомном порядке
isIvan :: User4 -> Bool
-- record field patterns
--isIvan user = login user == "Ivan" - без них
isIvan User4{ login = userName } = userName == "Ivan"
-- record update syntax
cloneIvan :: User4
cloneIvan = ivan { uid = 2 } -- User 2 "Ivan" "123"
-- records + sum - не безопасно но если не бояться эксепшен вот
--data Person 
--    = User  { uid :: Int, login :: String } 
--    | Admin { aid :: Int, login :: String }
--login :: Person -> String  -- after desugaring
--login (User  _ l) = l
--login (Admin _ l) = l
--ghci> uid $ Admin 0 "Vasya" 
-- почему небесопасно *** Exception: No match in record selector uid
-- total func - x и область определения совпадают, для всех х функция определена
-- partial func - существуют такие x на которых фунция не определена
-- {-# LANGUAGE DuplicateRecordFields #-} -- штука для решения проблемы одинаковых имен полец
data Man = Man { name :: String }
--data Cat = Cat { name :: String } - вот так нельяз, надо заменить так чтобы не было 2ух одинаковых
-- самый простой вариант по разному называть
-- {-# LANGUAGE RecordWildCards #-} еще оддно расширение для улучшения жизни но мне лень использовать расширения
-- я вручную напишу....
-- newtype data----------------------------------------------
-- newtype типа обертка 1 конструктор 1 поле
-- data  
data    Message = Message String
newtype Message1 = Message1 String
--зачем использовать newtype а примерме
-- вот если не использовать 
-- public key from secret key
{-# LANGUAGE InstanceSigs #-}
-- ниже кусок немного не компилится но мне лень пока
--derivePublicKey1 :: String -> String
--checkKeyPair1 :: (String, String) -> Bool
--checkKeyPair1 (secretKey1, publicKey1) 
--    = publicKey1 == derivePublicKey1 secretKey1
-- можно перепутать secret и public 
-- тогда на помощь приходит newtype
--newtype PublicKey = PublicKey String
--newtype SecretKey = SecretKey String
--derivePublicKey :: SecretKey -> PublicKey
--checkKeyPair :: (SecretKey, PublicKey) -> Bool
--checkKeyPair (secretKey, publicKey) = publicKey == derivePublicKey secretKey
-- Type classes - классы типов ---------------------------------
-- ad-hoc - специальный полиморфизм  это похоже на интерфейсы
class Printable p where  --список значений которые присущи данному классу типов
    printMe :: p -> String --параметров может быть больше только с определенным расширением 
data Foo = Foo | Bar --(два нуярных конструктора)
-- data Что хранится в нашем типе данных
-- class Что мы можем делать с нашими данными при постановке
-- Как же связать тип данных с классами типов  
instance Printable Foo where  -- мы инстанцируем класс типов принтбл для типа данныз фу  
    printMe Foo = "Foo" -- определяем принти для фу
    printMe Bar = "Bar (whatever)"
helloP :: Printable p => p -> String --функция которая принимает тип p и вовращает стоку
-- а какой это тип p - он должен инстанцировать класс типов принтбл 
-- => - этот знак значитт ограничение для того что после =>
helloP p = "Hello, " ++ printMe p ++ "!"
-- Пример 
-- ghci> helloP Bar -- "Hello, Bar (whatever)!"
--ghci> helloP True -- ошибка ноу инстанс
-- Классы типов базовые  ---------------------------
-- чет там все плохо компилится эту часть лекции не пишу
-- Eq 
-- надо определить == или /= чтобы работало. Пример
{-# LANGUAGE InstanceSigs #-} -- это нужно чтобы что то там с сигнатурами мудрить
threeSame :: Eq a => a -> a -> a -> Bool 
threeSame x y z = x == y && y == z
-- Ord
--data Ordering = LT | EQ | GT
-- simplified version of Ord class
--class Eq a => Ord a where --Ord является подклассом класса Eq потому что тоже 
--   compare              :: a -> a -> Ordering
--   (<), (<=), (>=), (>) :: a -> a -> Bool
--   compare x y
--       | x == y    =  EQ
--        | x <= y    =  LT
--        | otherwise =  GT
-- Num
-- у него определено + - * negate abs signum fromInteger 
-- Show -- ну все совсем устала короче это нужно чтобы можо было печатать в консоль 
-- получает а делает из этого стринг
-- Read --
-- получает строку пытается распарсить в a 
-- Примеры полиморфизма
subtract :: Num a => a -> a -> a
subtract x y = y - x
average :: Fractional a => a -> a -> a
average x y = (x + y) / 2
data TrafficLight2 = Red2 | Yellow2 | Green2 | Blue2
    deriving (Eq, Ord, Enum, Bounded, Show, Read) -- мнодество классов типов 
    -- которые мы хотим чтобы автоматически сделал инстансом для даты
-- ghci> show Blue - "Blue"
-- ghci> read "Blue" :: TrafficLight - Blue
-- ghci> Red == Yellow  -- (==) is from Eq  class - False
-- ghci> Red < Yellow   -- (<)  is from Ord class  - True
-- тут потому что кто первый того и тапки(объвлен)
-- ghci> fromEnum Green - 2
-- ghci> toEnum 2 :: TrafficLight - Green
-- нельзя дерайвить функции

-- Модулизация файлов
-- модули после прагм languages но до импортов
-- module имя модуля () where
-- в скобочках что можно экспортировать из модуля
-- лучше один раз увидеть (но я почему то не могу вставить скрин)
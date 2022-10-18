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
-- Maybe 
data Maybe a = Nothing | Just a  -- implemented in Prelude
-- Just - унарный конструктор
-- Nothing - нулярный конструктор - что то пошло не так
-- Either - типа Maybe но понятно что пошло не так
data Either a b = Left a | Right b  -- implemented in Prelude
-- рекурсивный тип 
data List a = Nil | Cons a (List a)
-- record syntax
data User4 = User4
    { uid      :: Int
    , login    :: String
    , password :: String 
    }
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
-- records + sum - не безопасно
-- total func - x и область определения совпадают, для всех х функция определена
-- partial func - существуют такие x на которых фунция не определена
-- {-# LANGUAGE DuplicateRecordFields #-} -- штука для решения проблемы одинаковых имен полец
-- newtype data
-- newtype типа обертка 1 конструктор 1 поле
-- data  
data    Message = Message String
newtype Message1 = Message1 String
-- Type classes - классы типов ---------------------------------
-- ad-hoc - специальный полиморфизм
-- Eq - ?? наверное что то типа иквелсов
-- Ord - что то про сравнеия
-- лениво дальше устала
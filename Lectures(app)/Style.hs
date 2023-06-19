-- Рекомендации к стилизации кода
module Style (main) where
    
import Numeric.Natural ( Natural )
import Lib ()
main :: IO()
main = putStrLn "Code style"


--  ПРОБЕЛЫ И ПУСТЫЕ СТРОКИ
--   Длина строки 80. Но иногда можно 100
--   Табы нельзя но можно 2 пробела примеры
sayHello :: IO ()
sayHello = do
  name <- getLine
  putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
--   1 пустая строка между определениями верхнего уровня.
--   0 пустых строк между сигнатурами типов и определениями функций.
--   | 1 пустая строка между определениями в объявлении экземпляра класса типов или внутри where
--   предложения или let блока, если определения большие. (что)
--   Можно добавить пустые строки внутрь большого do блока, чтобы отделить его логические части.
--   Можно выравнивать блоки кода дополнительными пробелами, если выравнивание подчеркивает общую структуру 
--   Пример
--
data WalletApiRecord route = WalletApiRecord
  { _test      :: route :- WTestApi             -- /test
  , _wallets   :: route :- WWalletsApi          -- /wallets
  , _accounts  :: route :- WAccountsApi         -- /accounts
  , _addresses :: route :- WAddressesApi        -- /addresses
  , _profile   :: route :- WProfileApi          -- /profile
  -- ...
  } deriving Generic
--  
--   Окружать бинарные операторы 1 пробелом обеих сторон: 3 + 5.
--   Можно не окружать, чтобы подчеркнуть группировку терминов: 2 + 2*2
--   При использовании каррирования(что) с бинарными операторами необходимо добавить 1 пробел между аргументом и операцией: (42 +)
--   Нужно удалить все конечные пробелы и добавить завершающую новую строку ко всем исходным файлам.
--   сюда https://editorconfig.org/

--  ИМЕНА
--   lowerCamelCase for functions, variables, and global constants.
--   UpperCamelCase for types.
--   Не использовать короткие имена, если значение не ясно из контекста
--   | НЕ надо использовать все буквы в аббревеатуре заглавными, например
--   лучше HttpServer вместо HTTPServer. Исключения - 2 или 3 буквенные типа IO, STM

--  RECORDS
--   я не поняла


--  КОММЕНТАРИИ
--   Длинные комментарии с правильной пунктуацией
--   Пишем сигнатуру типа для каждого определения верхнего уровня.
--   Комментируем каждую экспортируемую функцию и тип данных.
--   Комментируем каждую функцию верхнего уровня.
--   You must use Haddock syntax in the comments. (что)
--   какой-то пример
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send
  :: Socket      -- ^ Connected socket
  -> ByteString  -- ^ Data to send
  -> IO Int      -- ^ Bytes sent
--   По комменту должно быть понятно о чем функция
-- | Person representation used in address book.
data Person = Person
  { age  :: Int     -- ^ Age
  , name :: String  -- ^ First name
  }
--   Длинные комменты
data Record = Record
  { -- | This is a very very very long comment that is split over
    -- multiple lines.
    field1 :: Text

    -- | This is a second very very very long comment that is split
    -- over multiple lines.
  , field2 :: Int
  }
--   Отделять комментарии в конце строки от кода двумя пробелами.
--   Выравнивать комментарии для определений типов данных. 
data Parser = Parser
  Int         -- Current position
  ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * n + 9
  where
    salt = 453645243  -- Magic hash salt

--  ССЫЛКИ
--   нужно делать но если это может кому то быть нужным и только 1 раз

----------------------------------
-- Если не сложилось с stylish
--   | Напишите каждую LANGUAGEпрагму на отдельной строке, отсортируйте их по алфавиту 
--   и выровняйте по максимальной ширине среди них. 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
--   | Используйте единственное число при именовании модулей (например, используйте Data.Mapand
--   Data.ByteString.Internalвместо Data.Mapsand Data.ByteString.Internals).
--   Иногда допустимо использовать множественное число (например Types, , Instances).
--   Модули должны иметь явный экспорт
module Data.Set
  ( -- * The @Set@ type
    Set
  , empty
  , singleton

   -- * Querying
  , member
  ) where
--   отступ в 2 пробела для списка экспорта
--   отсортировать каждый раздел в алфавитном порядке. Однако в каждом разделе классы, типы данных и псевдонимы типов должны быть написаны перед функциями.
--   https://hackage.haskell.org/package/weeder штука чтобы убрать неиспользуемые импорты
--   остальное по импортам сложно 

-- ОБЪЯВЛЕНИЕ ДАННЫХ
--   Выровняйте конструкторы в определении типа данных. Примеры:
data Tree a
  = Branch a (Tree a) (Tree a)
  | Leaf

data HttpException
  = InvalidStatusCode Int
  | MissingContentHeader

data Person = Person
  { firstName :: String  -- ^ First name
  , lastName  :: String  -- ^ Last name
  , age       :: Int     -- ^ Age
  } deriving (Eq, Show)

--   Все функции верхнего уровня должны иметь сигнатуры типов.
--   Все функции внутри where должны иметь сигнатуры типов. 
--   Выравнивания если сигнатура длинная
putValueInState
  :: (MonadIO m, WithLogger m)
  => UserState
  -> Maybe Int
  -> AppConfig
  -> (Int -> m ())
  -> m ()
--   Много ограничений
parseTree
  :: ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentTokenToLit components) components
     )
  => Text
  -> Either (ParseError components) (Expr ParseTreeExt CommandId components)
--   Имена переменных длинные
putValueInState
  userState
  mValue@(Just x)
  Config{..}        -- { should go after ctor name without space
  valueModificator
    = do               -- note how this line uses 4-space indentation
  <code goes here>
--   Комментарий оператора
-- | Append a piece to the URI.
infixl 5 />
(/>) :: Uri -> PathPiece -> Uri
--  ПРАГМЫ хз
--  Список объявлений
numbers = [1, 2, 4]

exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
--   НЕ вставлять пробел после лямбды.
bar :: IO ()
bar =
  forM_ [1, 2, 3] $ \n -> do
    putStrLn "Here comes a number!"
    print n

foo :: IO ()
foo =
  alloca 10 $ \a ->
  alloca 20 $ \b ->
  cFunction a b
--   if else без выпендрежа
foo =
  if ...
  then ...
  else ...
--   с лямдами
foo =
  bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
--   с do
foo = do
  someCode
  if condition
  then do
    someMoreCode
    andMore
  else  -- you _may_ omit the `do` if the block is a one-liner
    return ()

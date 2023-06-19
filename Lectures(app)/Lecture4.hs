module Lecture4 (main) where
    
import Numeric.Natural ( Natural )
import Lib ()
main :: IO()
main = putStrLn "Basic typeclasses: Monoid. Functor. Applicative"


-- Полугруппа
-- Множество в котором можно взять бинарную замкнутую функцию
-- 2 аргумента множества и результат тоже принадлежит множеству
-- Надо определить бинарный оператор <>(меньше больше/ diamond)
--
--class Semigroup m where
--    (<>) :: m -> m -> m
--Associativity law for Semigroup: 
--  1. (x <> y) <> z ≡ x <> (y <> z) -- должна быть ассоциативность
-- Monoids
-- Это semigroup только с нейтральным элементом
--class Semigroup m => Monoid m where
--    mempty :: m --нейтральный пустой список 
--Identity laws for Monoid: 
--  2. x <> mempty ≡ x
--  3. mempty <> x ≡ x  
-- для list
--class Semigroup a where
--    (<>)    :: a -> a -> a
--    sconcat :: NonEmpty a -> a
--    stimes  :: Integral b => b -> a -> a -- приминяетнесколько раз
--class Semigroup a => Monoid a where
--    mempty  :: a
--    mappend :: a -> a -> a
--   mconcat :: [a] -> a
-- Хотим поддерживать разные операции diamond что делать?
-- instance Semigroup Int where
-- Which one to choose?
--   1. (<>) = (+)
--   2. (<>) = (*)
-- поддержим оба
newtype Sum     a = Sum     { getSum     :: a }
newtype Product a = Product { getProduct :: a }
instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)
--ghci> 3 <> 5 :: Sum Int      Sum { getSum = 8 }
--ghci> 3 <> 5 :: Product Int  Product { getProduct = 15 }
-- встроенные семигрупы
--newtype Max   a = Max   { getMax   :: a      }  -- max
-- ghci> Max 3 <> Max 10 <> Max 2                             Max { getMax = 10 }
--newtype Min   a = Min   { getMin   :: a      }  -- min
--ghci> Min 3 <> Min 10 <> Min 2                              Min { getMin = 2 }
--newtype Any     = Any   { getAny   :: Bool   }  -- ||
--ghci> Any True <> Any False <> Any True                     Any { getAny = True }
--newtype All     = All   { getAll   :: Bool   }  -- &&
--ghci> All True <> All False <> All True                     All { getAll = False }
--newtype First a = First { getFirst :: Maybe a}  -- first Just
--newtype Last  a = Last  { getLast  :: Maybe a}  -- last Just
--ghci> First Nothing <> First (Just 10) <> First (Just 1)   First { getFirst = Just 10 }
--ghci> Last (Just 2) <> Last (Just 1) <> Last Nothing       Last { getLast = Just 1 }

--Foldable
-- foldr :: (a -> b -> b) -> b -> [a] -> b - правая свертка
-- foldl :: (b -> a -> b) -> b -> [a] -> b - левая
--ghci> foldr (+) 0 [2, 1, 10]         13 = 2 + 1 + 10 + 0
--ghci> foldr (*) 3 [2, 1, 10]         60 = 2 * 1 * 10 * 3
-- Пишем свой foldr
-- foldr f ini [] = ini
-- foldr f ini (x:xs) = x `f` foldr f ini xs
--Пример
--ghci> foldr f x [a,b,c]
-- f a ( f b ( f c x ) )

--Functor
--class Functor f where               
--    fmap :: (a -> b) -> f a -> f b
-- 1. fmap id      ≡ id
-- 2. fmap (f . g) ≡ fmap f . fmap g
--instance Functor Maybe where
--    fmap :: (a -> b) -> Maybe a -> Maybe b
--    fmap f (Just x) = Just (f x)
--    fmap f Nothing  = Nothing
-- можно для сокращения писать с оператором
--(<$>) :: Functor f => (a -> b) -> f a -> f b
--(<$>) = fmap
--firstPostTitle = getPostTitle <$> findPost 1
-- -> -функтор
--Аппликативные функторы
--class Functor f => Applicative f where  -- f :: * -> *
--    pure  :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b
-- Maybe - апп. функтор
--ghci> Just (+3) <*> Just 9            Just 12
--ghci> pure (+3) <*> Just 10           Just 13
--ghci> Just (++"hahah") <*> Nothing    Nothing
--ghci> pure (++"what") <*> pure "lol" "lolwhat"
--for list
--ghci> [(*2), (+3)] <*> [1, 2, 3]
-- [2, 4, 6, 4, 5, 6]

--Alternative - моноид для апплик. функторов
--class Applicative f => Alternative f where
--    empty :: f a
--    (<|>) :: f a -> f a -> f a - оператор альт

--instance Alternative Maybe where
--    empty :: Maybe a
--    empty = Nothing

--    (<|>) :: Maybe a -> Maybe a -> Maybe a
--    Nothing <|> r = r - первое неошибочное значение (первый Just в цепочке)
--    l       <|> _ = l - ошибочное - эмпти

--Traversable - класс инстанциирующий 2 класса типа функтор и фолдабл
--class (Functor t, Foldable t) => Traversable t where
--    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
--    sequenceA :: Applicative f => t (f a) -> f (t a)
--Фантомный тип
--newtype Hash a = MkHash String -- `a` is a phantom type, not present in constructor
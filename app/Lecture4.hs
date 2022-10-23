module Lecture3 (main) where
    
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
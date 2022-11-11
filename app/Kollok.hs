module Kollok (main) where
    
import Numeric.Natural ( Natural )
import Lib ()
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"
main :: IO ()
main = putStrLn (greet "Tus") 
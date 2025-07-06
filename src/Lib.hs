module Lib
    ( runCalculator
    , someFunc
    , add
    , subtract
    , multiply
    , divide
    , modulate
    , power
    ) where

import Prelude hiding (subtract)
import Text.Read (readMaybe)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

add :: Int -> Int -> Int
add x y = x + y

subtract :: Int -> Int -> Int
subtract x y = x - y

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Int -> Int -> Int
divide x y = x `div` y

modulate :: Int -> Int -> Int
modulate x y = x `mod` y

power :: Int -> Int -> Int
power x y = x ^ y
-- ... existing functions unchanged ...

-- Safer version of calculate using Maybe
calculate :: Int -> Maybe (Int -> Int -> Int)
calculate switch = case switch of
    1 -> Just add
    2 -> Just subtract
    3 -> Just multiply
    4 -> Just divide
    5 -> Just modulate
    6 -> Just power
    _ -> Nothing

-- Interactive calculator
runCalculator :: IO ()
runCalculator = do
    putStrLn "Calculator Operations:"
    putStrLn "1. Add"
    putStrLn "2. Subtract"
    putStrLn "3. Multiply"
    putStrLn "4. Divide"
    putStrLn "5. Modulo"
    putStrLn "6. Power"

    putStr "Select operation (1-6): "
    opInput <- getLine

    putStr "Enter first number: "
    xInput <- getLine

    putStr "Enter second number: "
    yInput <- getLine

    -- Parse inputs safely
    case (readMaybe opInput, readMaybe xInput, readMaybe yInput) of
        (Just opCode, Just x, Just y) ->
            case calculate opCode of
                Just op ->
                    -- Handle division by zero separately
                    if opCode == 4 && y == 0
                    then putStrLn "Error: Division by zero"
                    else putStrLn $ "Result: " ++ show (op x y)
                Nothing -> putStrLn "Invalid operation selected"
        _ -> putStrLn "Invalid input. Please enter integers"

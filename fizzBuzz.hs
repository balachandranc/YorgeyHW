main = do
    putStrLn $ fmap fizzBuzz [1..100]


fizzBuzz::(Int a) => a -> [Char]
fizzBuzz n =
    let div = ( mod n 5 == 0, mod n 3 == 0 )
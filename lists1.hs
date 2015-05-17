myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = x : myInit xs
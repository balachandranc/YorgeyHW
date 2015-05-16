mconcat :: [[a]] -> [a]
mconcat [[]] = []
mconcat [[x]] = [x]
mconcat ([x]:xs) = x : mconcat xs
mconcat ((x:xs):outer) = x : mconcat (xs:outer)
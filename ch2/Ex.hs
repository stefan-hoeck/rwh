lastButOne :: [a] -> Maybe a
lastButOne (a:b:[]) = Just a
lastButOne (a:b:xs) = lastButOne (b : xs)
lastButOne _        = Nothing

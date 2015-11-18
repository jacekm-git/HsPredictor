module HsPredictor.Prelude where

sHead :: [a] -> Maybe a
sHead [] = Nothing
sHead (x:xs) = Just x

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) xs 0 = sHead xs
(!?) xs i = if i > length xs - 1
            then Nothing
            else Just $ xs !! i

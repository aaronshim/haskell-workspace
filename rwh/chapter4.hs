-- Selected exercises from Chapter 4 of Real World Haskell
import Data.Char

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just ((head . reverse) xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just ((reverse . tail . reverse) xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs =
    let (pre,suf) = break (not . f) $ dropWhile (not . f) xs --clean breaks
    in pre:(splitWith f suf)


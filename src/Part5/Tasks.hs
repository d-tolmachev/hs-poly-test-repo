module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x : xs) = f x $ myFoldr f acc xs

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f lst = myFoldr mapFunc [] lst
    where
        mapFunc x acc = f x : acc

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f lst = myFoldr concatMapFunc [] lst
    where
        concatMapFunc x acc = myFoldr (:) acc $ f x

myConcat :: [[a]] -> [a]
myConcat lsts = myFoldr concatFunc [] lsts
    where
        concatFunc lst acc = myFoldr (:) acc lst

myReverse :: [a] -> [a]
myReverse lst = myFoldl (flip (:)) [] lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p lst = myFoldr filterFunc [] lst
    where
        filterFunc x acc = if p x then x : acc else acc

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst = myFoldr partitionFunc ([], []) lst
    where
        partitionFunc x (plst, nplst) = if p x then (x : plst, nplst) else (plst, x : nplst)

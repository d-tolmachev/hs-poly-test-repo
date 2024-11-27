module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : (ff f $ f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq lst = tc_mostFreq (take 10 $ repeat 0) lst
    where
        tc_mostFreq freq_lst [] = snd $ maximum $ zip freq_lst [0..9]
        tc_mostFreq freq_lst (x : xs) = tc_mostFreq (updateFreqs freq_lst x) xs
        updateFreqs (x : xs) 0 = (x + 1) : xs
        updateFreqs freq_lst x = updateFreqs ((take (x `mod` 10) freq_lst) ++ ((freq_lst !! (x `mod` 10) + 1) : (drop (x `mod` 10 - 1) freq_lst))) (x `div` 10)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst = foldl (\ acc x -> if x `elem` acc then acc else x : acc) [] lst

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = foldl (updateGrok [] f) [] lst
    where
        updateGrok acc f [] x = (f x, [x]) : acc
        updateGrok acc f ((y, x_lst) : ys) x = if f x /= y then updateGrok ((y, x_lst) : acc) f ys x else ys ++ ((y, x : x_lst) : acc)

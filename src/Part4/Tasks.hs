module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = reversed $ reverse lst
    where
        reversed [] = REmpty
        reversed (x : xs) = reversed xs :< x

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    show lst = showsPrec 0 lst ""
    showsPrec _ lst = ("[" ++) . (showReverseList [] lst ++) . ("]" ++)
        where
            showReverseList acc REmpty = acc
            showReverseList [] (xs :< x) = showReverseList (show x) xs
            showReverseList acc (xs :< x) = showReverseList (((show x ++) . ((',' : acc) ++)) "") xs

instance (Eq a) => Eq (ReverseList a) where
    (/=) lhv rhv = not $ lhv == rhv
    (==) lhv rhv = isEq True lhv rhv
        where
            isEq False _ _ = False
            isEq acc REmpty REmpty = acc
            isEq _ REmpty rhv = False
            isEq _ lhv REmpty = False
            isEq acc (xs :< x) (ys :< y) = isEq (acc && x == y) xs ys

instance Semigroup (ReverseList a) where
    (<>) lhv rhv = mappendReverseLists lhv $ getReversed REmpty rhv
        where
            mappendReverseLists acc REmpty = acc
            mappendReverseLists acc (xs :< x) = mappendReverseLists (acc :< x) xs
            getReversed acc REmpty = acc
            getReversed acc (xs :< x) = getReversed (acc :< x) xs

instance Monoid (ReverseList a) where
    mempty = REmpty
    mconcat lsts = mconcatReverseLists REmpty lsts
        where
            mconcatReverseLists acc [] = acc
            mconcatReverseLists acc (x : xs) = mconcatReverseLists (mappendReverseList acc $ getReversed REmpty x) xs
            mappendReverseList acc REmpty = acc
            mappendReverseList acc (xs :< x) = mappendReverseList (acc :< x) xs
            getReversed acc REmpty = acc
            getReversed acc (xs :< x) = getReversed (acc :< x) xs

instance Functor ReverseList where
    fmap f lst = getReversed REmpty $ fmapReverseList REmpty f lst
        where
            fmapReverseList acc _ REmpty = acc
            fmapReverseList acc f (xs :< x) = fmapReverseList (acc :< f x) f xs
            getReversed acc REmpty = acc
            getReversed acc (xs :< x) = getReversed (acc :< x) xs

instance Applicative ReverseList where
    pure x = REmpty :< x
    (<*>) fs lst = getReversed REmpty $ apReverseList REmpty fs lst
        where
            apReverseList acc REmpty _ = acc
            apReverseList acc (fs :< f) lst = apReverseList (fmapReverseList acc f lst) fs lst
            fmapReverseList acc _ REmpty = acc
            fmapReverseList acc f (xs :< x) = fmapReverseList (acc :< f x) f xs
            getReversed acc REmpty = acc
            getReversed acc (xs :< x) = getReversed (acc :< x) xs

instance Monad ReverseList where
    return x = REmpty :< x
    (>>) m k = m >>= \_ -> k
    (>>=) lst f = getReversed REmpty $ bindReverseList REmpty lst f
        where
            bindReverseList acc REmpty _ = acc
            bindReverseList acc (xs :< x) f = bindReverseList (mappendReverseList acc $ f x) xs f
            mappendReverseList acc REmpty = acc
            mappendReverseList acc (xs :< x) = mappendReverseList (acc :< x) xs
            getReversed acc REmpty = acc
            getReversed acc (xs :< x) = getReversed (acc :< x) xs

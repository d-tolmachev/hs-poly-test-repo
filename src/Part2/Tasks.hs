module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) lhv rhv = add (evaluate lhv) (evaluate rhv)
    where
        add (IntConstant lhv) (IntConstant rhv) = IntConstant $ lhv + rhv
        add lhv rhv = BinaryTerm Plus lhv rhv
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) lhv rhv = sub (evaluate lhv) (evaluate rhv)
    where
        sub (IntConstant lhv) (IntConstant rhv) = IntConstant $ lhv - rhv
        sub lhv rhv = BinaryTerm Minus lhv rhv
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) lhv rhv = multiply (evaluate lhv) (evaluate rhv)
    where
        multiply (IntConstant lhv) (IntConstant rhv) = IntConstant $ lhv * rhv
        multiply lhv rhv = BinaryTerm Times lhv rhv
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant intValue) = IntConstant intValue
replaceVar varName replacement (Variable varName') = if varName == varName' then replacement else Variable varName'
replaceVar varName replacement (BinaryTerm op lhv rhv) = BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus lhv rhv) = lhv |+| rhv
evaluate (BinaryTerm Minus lhv rhv) = lhv |-| rhv
evaluate (BinaryTerm Times lhv rhv) = lhv |*| rhv
evaluate term = term

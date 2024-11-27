{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks (SparseMatrix(..), Matrix(matrixFromList, matrixWidth, matrixHeight, matrixElement), eye, zero, multiplyMatrix, determinant) where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    matrixFromList :: [[Int]] -> mx
    matrixWidth :: mx -> Int
    matrixHeight :: mx -> Int
    matrixElement :: mx -> Int -> Int -> Int
    matrixIsValidIndex :: mx -> Int -> Int -> Bool
    matrixIsValidIndex m col row = row >= 0 && col >= 0 && row < matrixHeight m && col < matrixWidth m
    matrixError :: mx -> ShowS -> a
    matrixError _ message = error $ (function . message) ""
        where
            function = ("Part6.Tasks.matrixElement" ++)
    matrixInvalidIndexError :: mx -> Int
    matrixInvalidIndexError m = matrixError m message
        where
            message = ("Invalid index" ++)
    matrixInvalidSizeError :: mx -> mx
    matrixInvalidSizeError m = matrixError m message
        where
            message = ("Invalid size" ++)

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    matrixFromList [[x]] = x
    matrixFromList _ = matrixInvalidSizeError 0
    matrixWidth _ = 1
    matrixHeight _ = 1
    matrixElement mx 0 0 = mx
    matrixElement mx _ _ = matrixInvalidIndexError mx

instance Matrix [[Int]] where
    matrixFromList elements = elements
    matrixWidth [] = 0
    matrixWidth (r : rs) = length r
    matrixHeight mx = length mx
    matrixElement mx col row
        | matrixIsValidIndex mx col row = mx !! row !! col
        | otherwise = matrixInvalidIndexError mx

instance Matrix (SparseMatrix Int) where
    matrixFromList [] = SparseMatrix 0 0 $ Data.Map.fromList []
    matrixFromList (r : rs) = SparseMatrix (length r) (length rs + 1) (Data.Map.fromList $ toSparseMatrix [] 0 (r : rs))
        where
            toSparseMatrix acc _ [] = acc
            toSparseMatrix acc row (r : rs) = toSparseMatrix (toSparseRow acc 0 row r) (row + 1) rs
            toSparseRow acc _ _ [] = acc
            toSparseRow acc col row (c : cs) = toSparseRow (if c /= 0 then ((col, row), c) : acc else acc) (col + 1) row cs
    matrixWidth mx = sparseMatrixWidth mx
    matrixHeight mx = sparseMatrixHeight mx
    matrixElement mx col row
        | matrixIsValidIndex mx col row = findWithDefault 0 (col, row) $ sparseMatrixElements mx
        | otherwise = matrixInvalidIndexError mx

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = matrixFromList [[if i == j then 1 else 0 | j <- [0 .. w - 1]] | i <- [0 .. w - 1]]

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = matrixFromList [[0 | _ <- [0 .. w - 1]] | _ <- [0 .. h - 1]]

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix lhv rhv
    | isValidSize lhv rhv = matrixFromList $ computeMultiplication [] 0 lhv rhv
    | otherwise = invalidSizeError
    where
        computeMultiplication acc row lhv rhv
            | row == matrixHeight lhv = acc
            | otherwise = computeMultiplication ((computeRow [] 0 row lhv rhv) : acc) (row + 1) lhv rhv
        computeRow acc col row lhv rhv
            | col == matrixWidth rhv = acc
            | otherwise = computeRow ((computeElement col row lhv rhv) : acc) (col + 1) row lhv rhv
        computeElement col row lhv rhv = sum [(matrixElement lhv i row) * (matrixElement rhv col i) | i <- [0 .. matrixHeight rhv - 1]]
        isValidSize lhv rhv = matrixWidth lhv == matrixHeight rhv
        invalidSizeError = error $ (("Part6.Tasks.multiplyMatrix" ++) . (": " ++) . ("Invalid matrix sizes" ++)) ""

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant mx
    | isValidSize mx = computeDeterminant [0 .. matrixHeight mx - 1] 0 mx
    | otherwise = invalidSizeError
    where
        computeDeterminant [col] row mx = matrixElement mx col row
        computeDeterminant cols row mx = sum [(-1)^i * (matrixElement mx col row) * (computeDeterminant (Prelude.filter (/= col) cols) (row + 1) mx) | (i, col) <- zip [0..] cols]
        isValidSize mx = matrixHeight mx == matrixWidth mx
        invalidSizeError = error $ (("Part6.Tasks.determinant" ++) . (": " ++) . ("Invalid matrix size" ++)) ""

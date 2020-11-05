module Main where

import Lib
import IO

main :: IO ()
main = someFunc

type Position = (Int, Int)
type FieldArray = UArray Position Char

-- новое игровое поле d*d, где d - длина слова
-- слово вставлено посередине поля
newField :: Int -> String -> FieldArray
newField d word = listArray ((1,1),(d,d)) (repeat '?') //
	(zip (zip (repeat mid) [1..d]) word)
	where mid = d `div` 2 + 1

-- границы поля
dimens a = d where ((1,1),(d,_)) = bounds a

-- соседние клетки
adjacents (y,x) table = let ((h1,w1),(h2,w2)) = bounds table in
	[ ((j, i), table ! (j, i))
	| (j,i) <- [(y-1,x),(y,x-1),(y+1,x),(y,x+1)]
	, (h1, h2) `inRange` j
	, (w1, w2) `inRange` i
	]

--соседние буквы
adjLetrs yx a =
	[ (ji, e)
	| (ji,e) <- adjacents yx a
	, e /= '?'
	]

-- пустые соседние клетки, ? - пустая клетка
emptyAdjLetrs :: FieldArray -> [Position]
emptyAdjLetrs a =
	[ yx
	| (yx,e) <- assocs a
	, e == '?'
	, any ((/='?').snd) $ adjacents yx a
	]









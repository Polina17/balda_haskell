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


--тип "Префиксное дерево" для хранения слов
newtype Trie = Trie [(String, Trie)]

instance Show Trie where
    showsPrec _ (Trie ts) = intern ts
        where
        intern ((inf,Trie tts):ts) =
            showString inf .
            paren tts .
            commaTail tts ts
        paren [] = id
        paren tts = showParen True $ intern tts
        commaTail _ [] = id
        commaTail [] ts = (:) ',' . intern ts
        commaTail _ ts = intern ts


instance Read Trie where
    readsPrec _ xs = [(Trie ts, xs')]
        where
        (ts, xs') = intern "" xs
        intern acc "" = case acc of
            "" -> ([], "")
            _ -> ([(reverse acc, Trie [])], "")
        intern acc (x:xs) = case x of
            ',' -> let (ts,xs') = intern "" xs in
                ((reverse acc, Trie []) : ts, xs')
            '(' -> let (tts,xs'') = intern "" xs; (ts,xs') = intern "" xs'' in
                ((reverse acc, Trie tts) : ts, xs')
            ')' -> case acc of
                "" -> ([], xs)
                _ -> ([(reverse acc, Trie [])], xs)
            _ -> intern (x:acc) xs


--Вставка слова в дерево
insert :: Trie -> String -> Trie
insert (Trie ts) xs = Trie $ intern ts xs
    where
    intern :: [(String, Trie)] -> String -> [(String, Trie)]
    intern [] xs = [(xs, Trie [])]
    intern tts@(([],_):_) [] = tts
    intern tts@(yys@(ys,Trie yts):ts) xs = case prefMatch xs ys of
        ([],_,_)
            | xs>ys -> yys : intern ts xs
            | otherwise -> (xs, Trie []) : tts
        (pref,[],[]) -> case yts of
            [] -> tts
            _ -> (ys, Trie $ intern yts []) : ts
        (pref,xs',[]) -> case yts of
            [] -> (ys, Trie [([], Trie []), (xs', Trie [])]) : ts
            _ -> (ys, Trie $ intern yts xs') : ts
        (pref,xs',ys')
            | xs'<ys' -> (pref, Trie $ (xs', Trie []) : [(ys', Trie yts)]) : ts
            | otherwise -> (pref, Trie $ (ys', Trie yts) : [(xs', Trie [])]) : ts


dropPrefixes :: Trie -> Trie
dropPrefixes (Trie ts) = Trie $ intern ts
    where
    intern [] = []
    intern (([],Trie []):ts) = intern ts
    intern ((pref,Trie tts):ts) = (case intern tts of
        [(suf,yts)] -> (pref ++ suf, yts)
        itts -> (pref, Trie itts)) : intern ts


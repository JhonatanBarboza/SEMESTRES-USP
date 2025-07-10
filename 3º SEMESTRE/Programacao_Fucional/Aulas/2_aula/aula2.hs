main = do 
    putStrLn "HW"
    putStrLn $ show $ 5 +-=-@
    putStrLn $ show $ mapa dobro x a
    putStrLn $ show $ mapa (*2) a
    putStrLn $ show $ mapa (²) $ mapa (+1) a
    putStrLn $ show $ pega 3 a
    putStrLn $ show $ soma  $ mapa (²) $ filta impar a -- soma dos quadrados dos impares de a
    putStrLn $ show $ soma  $ mapa (2^) $ pega 3 a $ filta impar a -- soma da potencia de 2 trez primeiros impares de a
    putStrLn $ show $ soma  $ mapa (2^) $ pega 3 a $ filta impar $ pega 3 a -- soma da potencia de 2 trez primeiros digitos de a
    putStrLn $ show $ 5 `pertence` a
    putStrLn $ show $ 'f' `pertence` ['A', 'B', 'c']
    putStrLn $ show $ 



a = [1, 2, 3, 4]

-- PREFIX E INFIX 

x = 42
y = x+5
z = (+) x 5 -- operação como prefix se estiver entre parenteses

f a b = a²+b*2
a +-=-@ b = a²+b*2 -- os operadores são função, neste aso esta sendo usado como infix

impar x=(mod x 2) == 1 -- mod e == são funções tambem
impar x=(x `mod` 2) == 1 -- se colocar entre crase torna infix 


-- FUNÇÃO

add :: Integer -> Integer -> Integer
add a b = a + b
g x = add 5 x  -- função f retorna um inteiro x
h = add 5 -- igual o de cima 

-- são todos equivalentes uma função que dado um inteiro retprna um int + 5
m x = (+) 5 X
n = (+) 5
p = (+5)


-- DOBRO DE UMA LISTA 

-- multiplica todos os elementos por dois

mapa :: (a -> b) -> [a]->[b]
mapa _ [] = []
mapa f (x:xs) = f x:mapa f xs


-- FIlTRA 

filtra :: (a->Bool) -> [a] -> [a]
filtra _ [] = []
filtra teste (x:xs)
    | teste x = x:filtra teste xs
    | otherwise = filtra teste xs


pega :: Integer => a -> [b] -> [b]
pega 0 = []
paga _[] = []
pena n (x:xs) = x:pega (n-1) xs

-- Num é uma classe onde os pitos podem ser somados 
-- qual o tipo 
soma :: (Num a) => [a] -> a
soma [] = 0
soma (x:xs) = x:soma xs


-- Eq é a classe de igualdade, ou seja todos os tipo que podem ser comparados 
-- se uma elemento pertence a uma de elementos (o generico não vai funcionar por que ele é generico de mais)
pertence :: (Eq a) => a -> [a] -> Bool
- `pertence` [] = False
e `pertence` (x:xs)
    | e == x Trie
    | otherwise = e `pertence` xs


-- tenho que saber comparar e somar ou seja para numeros apenas 
pertence2 :: (Eq a, Num a) => a -> [a] -> Bool 
- `pertence` [] = False
e `pertence` (x:xs)
    | e == x Trie
    | otherwise = (e+1) `pertence` xs


(concatena) :: [a] -> [a] -> [a]
[] concatena 1 = 1
(x:xs) concatena 1 = x:(xs concatena 1) 



-- quick sort
-- Ord os tipos que podem ser ordenados 
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (pivot:xs) = menores concatena iguais concatena maiores 
  where
    menores = qs $ filtra (<pivot) xs
    iguais = pivot:filtra (== pivot) xs 
    maiores = qs $ filtra (>pivot) xs 



-- otimizado

qs' :: (Ord a) => ([a] -> [a]) -> [a] -> [a]
qs' [] = []
qs' pp 1 = menores concatena iguais concatena maiores 
  where
    pivot = pp 1
    menores = qs' pp $ filtra (<pivot) xs
    iguais = filtra (== pivot) xs 
    maiores = qs' pp $ filtra (>pivot) xs 
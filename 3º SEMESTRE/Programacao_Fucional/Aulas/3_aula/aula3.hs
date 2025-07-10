-- Crivo de eratosteles

main = do 
    putStrLn "HW"
    putStrLn $ show $ pega 10 $ primos
    putStrLn $ show $ pegaEqto (<100) $ primos -- imprime os primos menores que 100
    putStrLn $ show $ soma $ pegaEqto (<200) $ ignoraEqto (<100) $ primos -- imprime a soma dos primos menores que 100

    putStrLn $ show $ sum $ takewhile (<100) $ ignoraEqto (<100) $ primos -- imprime a soma dos primos menores que 100

    putStrLn $ show $ head $ ignoraEqto (<10000) $ primos -- imprime o primeiro primo maior que 100
    putStrLn $ show $ pega 10 $ ignoraEqto ((<10000). segundo) $ zipa [1..] primos -- imprime os 10 primeiros primos maiores que 100
    putStrLn $ show $ zipa [1, 2, 3, 6] "Adenilso"
    putStrLn $ show $ pega 10 $ zipaSoma [1..] primos -- imprime os 10 primeiros primos maiores que 100
    putStrLn $ show $ pega 10 $ zipaCom (+) [1..] primos -- imprime os 10 primeiros primos maiores que 100
    putStrLn $ show $ pega 10 $ zipaCom (+) uns primos




-- lista infinita de uns
uns = 1:uns
-- primos :: [Integer]
primos = crivo [2..]m -- [2,3,5,7,9,11,13,15,17,19,21,23,25]-- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]
    where 
        --crivo(x:xs) = x:(crivo $ filtro (naoEhMultiplo x) xs) -- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]
        -- utilizando lambida 
        -- crivo(x:xs) = x:(crivo $ filtro (\y -> y `mod` x /= 0) xs) -- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]
        -- usando operador de ponto
        crivo(x:xs) = x:(crivo $ filtro ((\=0).(`mod` x)) xs) -- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]

-- função ponto
--(.) :: (a -> b) -> (c -> a) -> (c -> b)

primos = crivo [2..] -- [2,3,5,7,9,11,13,15,17,19,21,23,25]-- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]

-- eguanto o paramentro for verdade 
pagaEqto :: (a -> Bool) -> [a] -> [a] -- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]
pegaEqto _ [] = []
pegaEqto p (x:xs)
    | p x = x:pegaEqto p xs
    | otherwise = pegaEqto p xs

-- iguinora ate satisfazer o parametro 
ignoraEqto :: (a -> Bool) -> [a] -> [a] -- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]
ignoraEqto _ [] = []
ignoraEqto p (x:xs)
    | p x = ignoraEqto p xs
    | otherwise = x:xs

-- soma os elementos de uma lista
soma :: (Num a ) => [a] -> a
soma [] = 0
soma (x:xs) = x + soma xs
-- soma [1,2,3] = 1 + soma [2,3] = 1 + 2 + soma [3] = 1 + 2 + 3 + soma [] = 0

naoEhMultiplo :: (Integer -> Integer -> Bool) -- (2,3) -> Bool
naoEhMultiplo x y = y `mod` x/=0

filtro :: (a -> Bool) -> [a] -> [a] -- (2,3) -> [3,5,7,9,11,13,15,17,19,21,23,25]
filtro _ [] = []
filtro p (x:xs)
    | t x = x:filtro t xs
    | otherwise = filtro t xs
    
pega :: Integer ->[a] -> [a]
pega 0 (x:xs) = x:xs


-- concatena duas listas
zipa :: [a] -> [b] -> [(a,b)]
zipa [] _ = []
zipa _ [] = []
zipa (x:xs) (y:ys) = (x,y):zipa xs ys

primeiro :: (a,b) -> a
primeiro (x, _)

segundom :: (a,b) -> b
segundom (_, y) = y

zioaSoma :: (Num a) => [a] -> [a] -> [a]
zioaSoma [] _ = []
zioaSoma _ [] = []
zioaSoma (x:xs) (y:ys) = (x+y):zioaSoma xs ys


zipaCom :: (a -> b -> c) -> [a] -> [b] -> [c]
zipaCom _ [] _ = []
zipaCom _ _ [] = []
zipaCom f (x:xs) (y:ys) = op x y:zipaCom op xs ys
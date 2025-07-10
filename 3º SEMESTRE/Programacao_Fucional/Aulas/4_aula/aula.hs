-- (fx): (map f xs)
-- (:)(fx)(map f xs)
-- ((:).f)(map fx)

-- map=foldr ((:).f)[]

main = do
    putStrLn "HW"
    putStrLn $ show a 
    putStrLn $ show b
    putStrLn $ show c
    putStrLn $ show d
    putStrLn $ show e
    putStrLn $ show f
    putStrLn $ show f'

f = do 
    la <- getLine
    let a = read la
    putStrLn $ show (a+1)

f' = getLine >>= \la -> let a = read la in putStrLn $ show (a+1)

-- monad 
h :: IO Integer
h = do
    la <- getLine
    let a = read la :: Integer
    return (a+1)

-- monad generalizada
(>>=) :: Monad m => m a -> (a -> m b) -> m b



a = [1..10]
-- as duas linhas a seguir sõa equivalentes
b = map (²) $ filter ((==1) . (`mod` 2)) a
c = [x² | x <- a, x `mod` 2 == 1]
d = [x+y | x <- a, x `mod` 3 == 0, y <- a, x-y < 15]
e = [(x, y, x*y) | x <- a, y<-a]

-- quiksort
qs [] = []
qs (x:xs) = qs meneres ++ iguais ++ maiores
    where
        menores = filter (< x) xs
        iguais = filter (== x) xs
        maiores = filter (> x) xs

crivo = p[2..]
    where p (x:xs) = x : p [y | y <- xs, y `mod` x /= 0]








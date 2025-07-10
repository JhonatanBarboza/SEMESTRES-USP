main = do
    putStrLn $ show a
    putStrLn $ show $ soma a
    putStrLn $ show $ somaPos a
    putStrLn $ show $ somaNeg a
    putStrLn $ show $ somaPar a

    putStrLn $ show $ somaSe "Pos" a 
    putStrLn $ show $ somaSe "Neg" a 
    putStrLn $ show $ somaSe "Par" a 

    putStrLn $ show $ somaTeste ehPositivo a
    putStrLn $ show $ somaTeste ehNegativo a
    putStrLn $ show $ somaTeste ehPar a 

    putStrLn $ show $ somaTeste (\x -> mod x 2==1) a -- é um teste feito sobre quem esta chamando 

    putStrLn $ show $ operaTeste (\x -> x > 0 ) multiplica 1 a  
    putStrLn $ show $ operaTeste (\x -> Truwe ) multiplica 1 a  -- multiplicar todos os elementos de uma lista  

    putStrLn $ show $ operaTeste (>0) (*) 1 a  -- codigo final

    putStrLn $ show $ soma $ filtra (\x -> x >0) a -- filnal 


-- Haskell Curry (toda função tem um unico parametro)
-- type (tipos)
-- ty class (classe de tipo)

 k :: Integer -- sempre que ele achar o k ele sera do tipo integer é um int infinito 
 k = 5

m:: Int -- do tipo Int
m = 5

multiplica :: Integer -> Integer -> Integer
multiplica x = \y -> x*y


-- multiplica x y = x*y
-- multiplica'' = \x -> (\y ->x*y)

a :: [Integer]
a[1,2,3, 234, 234, 345]

soma []=0
soma(x:xs)=x+soma xs -- soma da cabeça com acalda

somaPos []=0
somaPos (x:xs)
    | x>0=x+somaPos xs
    | otherwise = somaPos xs


somaNeg []=0
somaNeg (x:xs)
    | x>0=x+somaNeg xs
    | otherwise = somaNeg xs


somaPar []=0
somaPar (x:xs)
    | x>0=x+somaPar xs
    | otherwise = somaPar xs

somaSe cond []=0 -- melhor mais ainda presisa ficar definaindo novas condições 
somaSe cond (x:xs)
    | cond == "Pos" && X>0 = x+somaSe cond
    | cond == "Neg" && X<0 = x+somaSe cond
    | cond == "Par" && mod = 2==0 =x+somaSe cond xs
    otherwise = somaSe cond xs

somaTeste :: (Tnteger -> Bool) -> [Integer] -> [Integer]
somaTeste teste []=0 -- ainda melhor não presisa ficar redefinindo 
somaTeste teste (x:xs)
    | teste x=x + somaTeste xs -- esta somando qualquer coisa 
    | otherwise = somaTeste teste xs


-- função de alta ordem 
ehPositovo :: Integer -> Bool
ehPositivo x=x>0 
ehPos =\x -> x>0 -- utilizando o conseito de lambida
ehNegativo x=x<0
ehNeg =\x -> x<0 -- utilizando o conseito de lambida


prodTeste teste []=0
prodTeste teste (x:xs)
    | teste x=x * proTeste teste xs 
    | otherwise = proTeste teste  xs


operaTeste teste op neutro [] = neutro 
operadorteste teste op neutro [] (x:xs)
    | teste x = op x $ operaTeste  teste op neutro xs
    | otherwise = operaTeste teste op neutro xs


filtra teste [] = []
filtra  teste (x:xs)
    | teste x = x:r
    | otherwise = r
        where
            r=filtra teste xs

filtra :: (a -> Bool) -> [a] -> [a] -- filtro generico a pode ser qualquer coisa 
filtra _ [] = [] -- se nao usar o paramentro na clausula colocala com _
filtra  teste (x:xs)
    | teste x = x:r
    | otherwise = r
        where
            r=filtra teste xs



polinomio :: Int -> Int
polinomio x = x*x + x*10 + 2

quadrado :: Int -> Int
quadrado x = x*x

soma :: Int -> Int -> Int
soma a b = a+b

--quadrado
areaq :: Int -> Int 
areaq x = x*x 

-- retungulo
arear :: Int -> Int -> Int 
arear a b = a * b

-- circulo
areaC :: Float -> Float
areaC r = pi * r*r

-- hipotanusa
hipo :: Float -> Float -> Float
hipo a b = sqrt (a*a + b*b)

comp_quadrados :: Int -> Int -> Bool
comp_quadrados a b = (quadrado a) > (quadrado b)

ifelse :: Int -> Int 
ifelse x = if x > 10 then x else x*2 -- o else é obrigatorio pois as funçoes sempre deverm retornar alguma coisa

hello :: String -> String --recebe uma string e retorna uma string
hello x = "Hello "++x --concatena a string recebida com a string "Hello"

-- lista são homogeneas dsddd seja todos os elementos devem ser do mesmo tipo
opLista :: [Int] -> [Int]
opLista xs = [2 * x | x <- xs, x == 10] -- s={2*x | x pertence a xs, x == 10}

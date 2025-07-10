-- Função para obter os divisores próprios de um número (excluindo o próprio número)
divisoresProprios :: Int -> [Int]
divisoresProprios n = [x | x <- [1..n-1], n `mod` x == 0]

-- Função para calcular a soma dos divisores próprios
somaDivisores :: Int -> Int
somaDivisores n = sum (divisoresProprios n)

-- Classifica um número como defeituoso, perfeito ou abundante
classificarNumero :: Int -> String
classificarNumero n
    | soma == n  = "perfeito"
    | soma > n   = "abundante"
    | otherwise  = "defeituoso"
    where soma = somaDivisores n

-- Conta quantos números no intervalo são defeituosos, perfeitos e abundantes
contarClassificacoes :: Int -> Int -> (Int, Int, Int)
contarClassificacoes a b = 
    let numeros = [min a b .. max a b]
        classificacoes = map classificarNumero numeros
        defeituosos = length $ filter (== "defeituoso") classificacoes
        perfeitos = length $ filter (== "perfeito") classificacoes
        abundantes = length $ filter (== "abundante") classificacoes
    in (defeituosos, perfeitos, abundantes)

-- Função principal que lê os números e imprime os resultados
main :: IO ()
main = do
    a <- readLn
    b <- readLn
    let (defeituosos, perfeitos, abundantes) = contarClassificacoes a b
    putStrLn $ show defeituosos
    putStrLn $ show perfeitos
    putStrLn $ show abundantes
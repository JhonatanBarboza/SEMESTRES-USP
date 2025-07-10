-- Função para calcular a soma dos quadrados de uma lista de números
somaQuadrados :: [Int] -> Int
somaQuadrados lista = sum [x^2 | x <- lista]

-- Função principal
main :: IO ()
main = do
    let numeros = [1, 2, 3, 4, 5]
    let resultado = somaQuadrados numeros
    putStrLn ("A soma dos quadrados dos números " ++ show numeros ++ " é " ++ show resultado)
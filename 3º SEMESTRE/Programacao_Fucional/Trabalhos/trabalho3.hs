import System.IO

-- Função para verificar se um número é primo
isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | n == 2    = True
    | even n    = False
    | otherwise = null [x | x <- [3,5..floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- Função para obter a lista de primos no intervalo [x, y]
primesInRange :: Int -> Int -> [Int]
primesInRange x y = filter isPrime [x..y]

-- Função para calcular os intervalos entre primos consecutivos
primeGaps :: [Int] -> [Int]
primeGaps [] = []
primeGaps [_] = []
primeGaps (p1:p2:ps) = (p2 - p1) : primeGaps (p2:ps)

-- Função principal
main :: IO ()
main = do
    input <- getLine
    let x = read input :: Int
    input <- getLine
    let y = read input :: Int
    
    let primes = primesInRange x y
    let gaps = primeGaps primes
    
    if null gaps
        then putStrLn "0"
        else print (maximum gaps)
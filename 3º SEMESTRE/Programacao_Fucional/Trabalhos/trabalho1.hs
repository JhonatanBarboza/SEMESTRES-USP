étriangulo :: Int -> Int -> Int -> Bool -- Verifica se é triangulo
étriangulo a b c  = (a+b)>=c && (a+c)>=b && (b+c)>=a

perimetro :: Int -> Int -> Int -> Int --calcula o perimetro do trianlulo
perimetro a b c = a + b + c

areatriangulo :: Int -> Int -> Int -> Double -- Calcula a area de um triangulo
areatriangulo a b c = sqrt (s * (s - fromIntegral a) * (s - fromIntegral b) * (s - fromIntegral c))
  where
    s = fromIntegral (perimetro a b c) / 2

main :: IO ()
main = do 

    --Lendo cateto adijasente 
    entradaA <- getLine
    let ca = read entradaA :: Int 
    --Lendo cateto oposto
    entradaB <- getLine
    let co = read entradaB :: Int
    --Lendo hipotenusa
    entradaC  <- getLine
    let hip = read entradaC :: Int

    if étriangulo ca co hip --Se for triangulo calculara area 
        then do
            let area = areatriangulo ca co hip
            putStrLn (show area)
        else
            putStrLn "-"

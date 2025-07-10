import System.IO

data Curso = BCC | BSI | EC
    deriving (Show, Read)

data Aluno1 = Aluno1 {
    nome :: [Char],
    nusp :: Integer,
    curso :: Curso,
    notas :: [Float]
}
    deriving (Show, Read)

-- é um contrutor de Aluno
a1 :: Aluno
-- Aluno é um tipo de dado que representa um aluno
a1 = Aluno { 
    nome :: "João",
    nusp :: 123456,
    curso :: BCC,
    notas :: [10.0, 9.5, 8.0]
}

a2 = Aluno { 
    nome :: "Maria",
    nusp :: 654321,
    curso :: BSI,
    notas :: [7.0, 8.5, 9.0]
}

-- Funciona contando que não use os outros campos, mas vai dar walning
a3 = Aluno { 
    notas :: [7.0, 8.5, 9.0]
}



main = do 
    h <- openFile "aula5.txt" ReadMode -- o equivalente fileopen em c
    contents <- hGetContents h  -- lê o arquivo inteiro
    let ls = lines contents -- transforma o conteúdo em uma lista de linhas
    let l = ls -- pega o tamanho da lista de linhas
    putStrLn $ show l -- imprime o tamanho da lista 


    hClose h -- fecha o arquivo
    
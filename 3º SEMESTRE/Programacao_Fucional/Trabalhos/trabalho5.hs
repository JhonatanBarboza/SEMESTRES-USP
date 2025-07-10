-- Beatriz Alves dos Santos 15588630
-- Jhonatan Barboza da Silva 15645049
-- Kevin Ryoji Nakashima 15675936

-- Inclusão das bibliotecas necessárias
import Data.List (sortBy)
import Data.List (nub)
import Data.Ord (comparing)
import System.IO

-- Declaração da "Struct"
data Dados = Dados {
    country :: String,
    confirmed :: Int,
    deaths :: Int,
    recovery :: Int,
    active :: Int
}deriving (Show, Read, Eq)

-- Função principal
main = do
    
    -- Pegando a entrada do usuário
    input <- getLine
    
    -- Convertendo a string de entrada para n1, n2, n3 e n4
    let [n1, n2, n3, n4] = map read (words input) :: [Int]
    
    -- Abrindo o arquivo para leitura
    csv <- openFile "dados.csv" ReadMode
    
    -- Extrai o conteúdo do csv e coloca em "dados"
    dados <- hGetContents csv
    
    -- Converte a string em uma lista de strings, separando linha por linha
    let linhas = lines dados
    
    -- FUNÇÃO 1:
    putStrLn $
        show $                          -- Int -> String
        sum $                           -- [Active] -> Int 
        map active $                    -- [Dados] -> [Active]
        filter ((>= n1) . confirmed) $  -- [Dados] -> [Dados]
        map converte_string linhas      -- [String] -> [Dados]
        
    -- FUNÇÃO 2:
    putStrLn $
        show $                          -- Converte Int para String
        sum $                           -- Soma os valores da lista
        map deaths $                    -- Converte para uma lista de inteiros, correpondente ao campo Deaths da lista
        take n3 $                       -- Reduz a lista para os primeiros n3 elementos
        sortBy (comparing confirmed) $  -- Ordena de maneira crescente, em relação ao campo Confirmed, a lista de Dados
        take n2 $                       -- Reduz a lista para os primeiros n2 elementos
        reverse $                       -- Inverte a ordem da lista (Agora decrescente)
        sortBy (comparing active) $     -- Ordena de maneira crescente, em relação ao campo Active, a lista de Dados
        map converte_string linhas      -- [String] -> [Dados]
    
    -- FUNÇÃO 3:
    putStr $
        unlines $                       -- Converte uma lista de Strings em uma String única, mas com \n separando as strings pertencentes a lista
        map country $                   -- Converte para uma lista de inteiros, correpondente ao campo Country da lista
        nub $                           -- Tira os valores repetidos de uma lista
        sortBy (comparing country) $    -- Ordena de maneira crescente, em relação ao campo Country, a lista de Dados
        take n4 $                       -- Reduz a lista para os primeiros n4 elementos
        reverse $                       -- Inverte a ordem da lista (Agora decrescente)
        sortBy (comparing confirmed) $  -- Ordena de maneira crescente, em relação ao campo Confirmed, a lista de Dados
        map converte_string linhas      -- [String] -> [Dados]
    
    -- Fechando o arquivo
    hClose csv
    
-- Converte uma linha do arquivo csv em uma variável do tipo Dados
converte_string :: String -> Dados
converte_string str =
    let [country, confirmed, deaths, recovery, active] = split str
    in Dados country (read confirmed) (read deaths) (read recovery) (read active) 
    
-- Converte uma String, separada por vírgulas, em uma lista de Strings
split :: String -> [String]
split = foldr step [""]
  where
    step ',' acc     = "" : acc
    step c   (x:xs)  = (c:x) : xs
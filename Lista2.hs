import Data.Char

--double :: [Int] -> [Int] dobra os elementos de uma lista

dobraElementos :: [Int] -> [Int]
dobraElementos [x] = [2*x]
dobraElementos (x:xs) = 2*x : dobraElementos xs

--member :: [Int] -> Int -> Bool verifica se um inteiro dado como argumento está na lista

pertence :: [Int] -> Int -> Bool 
pertence [] _ = False
pertence (x:xs) n   | (n==x) = True
                    | otherwise = pertence xs n

membroLista :: [Int] -> Int -> Bool 
membroLista [] _ = False
membroLista (x:xs) n   | (n==x) = True
                       | otherwise = pertence xs n

--digits :: String -> String resulta em uma lista que contém apenas os dígitos da lista dada como argumento

digitoLista :: [Char] -> [Char] 
digitoLista [] = []
digitoLista (x:xs) 
    | isDigit x = x : digitoLista xs
    | otherwise = digitoLista xs


--sumPairs :: [(Int,Int)]->[Int] resulta em uma lista com a soma dos pares da lista dada como argumento

somarParesLista :: [(Int,Int)] -> [Int]
somarParesLista [] = []
somarParesLista ((x1,x2):xs) = x1+x2 : somarParesLista xs


--Parte 2
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), ("Fernando","Introduction to Programming with Python"), ("Fernando","Programming in Haskell")]

--Desenvolva as seguintes funções (sem usar compreensão de listas):
--livros :: BancoDados -> Pessoa -> [Livro]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((p,l) : xs) pessoa
   | p == pessoa = l : livros xs pessoa
   | otherwise = livros xs pessoa

--emprestimos :: BancoDados -> Livro -> [Pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos ((p,l) : xs) livro
    | l == livro = p : emprestimos xs livro
    | otherwise = emprestimos xs livro

--emprestado :: BancoDados -> Livro -> Bool

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((p,l) : xs) livro
    | l == livro = True
    | otherwise = emprestado xs livro 

--qtdEmprestimos :: BancoDados -> Pessoa -> Int

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos ((p,l) : xs) pessoa
    | p == pessoa = 1 + qtdEmprestimos xs pessoa
    | otherwise = qtdEmprestimos xs pessoa


--emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar banco p l = (p,l) : banco

--devolver :: BancoDados -> Pessoa -> Livro -> BancoDados

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver ((p,l) : xs) pessoa livro
   | (p == pessoa && l == livro) = devolver xs pessoa livro
   | otherwise = (p,l) : devolver xs pessoa livro

--Redefina as seguintes funções, usando compreensão de listas
--membro :: [Int] -> Int -> Bool

membro :: [Int] -> Int -> Bool
membro l n = length [ 1 | x <- l, x == n] /= 0

--livros :: BancoDados -> Pessoa -> [Livro]

livros2 :: BancoDados -> Pessoa -> [Livro]
livros2 banco pessoa = [ snd l | l <- banco, fst l == pessoa]

--emprestimos :: BancoDados -> Livro -> [Pessoa]

emprestimos2 :: BancoDados -> Livro -> [Pessoa]
emprestimos2 banco livro = [ fst l | l <- banco, snd l == livro]

--emprestado :: BancoDados -> Livro -> Bool

emprestado2 :: BancoDados -> Livro -> Bool
emprestado2 banco livro = length [ 1 | (p,l) <- banco, l == livro] /= 0

--qtdEmprestimos :: BancoDados -> Pessoa -> Int

qtdEmprestimos2 :: BancoDados -> Pessoa -> Int
qtdEmprestimos2 banco pessoa = length [ 1 | (p,l) <- banco, p == pessoa]

--emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados

emprestar2 :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar2 banco pessoa livro = (pessoa, livro) : banco







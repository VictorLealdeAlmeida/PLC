import Data.Char

-- Dê uma definição de ou-exclusivo que funciona informalmente da seguinte maneira: "o ou-exclusivo entre x e y será True se x é True e y é False or x é False e y é True"
--Usando literais no lado esquerdo, podemos escrever uma função para uma tabela-verdade como uma definição em Haskell. Complete a seguinte definição de ou-exclusivo neste estilo

exOr :: Bool -> Bool -> Bool
exOr a b | a == b = False
                 | otherwise = True

--Apresente duas definições distintas da função nAnd :: Bool -> Bool -> Bool que retorna o resultado True exceto quando os dois argumentos são True.

nAnd :: Bool -> Bool -> Bool
nAnd a b | (((a == True) || (b == True)) && a /= b) = True
         | otherwise = False

nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = False
nAnd2 a b = True

--Defina uma função que converte letras minúsculas em maiúsculas e retorna caracteres não modificadas no caso de maiúsculos

upper :: Char -> Char
upper x = toUpper x

--Defina a função charToNum :: Char -> Int que converte um dígito como '8' no inteiro 8, por exemplo

charToNum :: Char -> Int
charToNum x = digitToInt x 
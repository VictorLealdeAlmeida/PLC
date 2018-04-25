------ Exercícios - listas e tipos algébricos

--Defina uma função que, dados dois números x e y, retorne como resultado o m.d.c. de x e y.

mdc :: Int -> Int -> Int
mdc m 0 = m
mdc m n = mdc n (mod m n)

--Defina uma função que, dado um número inteiro positivo x, verifique se x é primo ou não. Lembre-se de utilizar o crivo de Eratóstenes por razões de otimização.

ehPrimo :: Int -> Bool
ehPrimo n = pertenceF (filtroPrimo[2 .. n]) n

filtroPrimo :: [Int] -> [Int]
filtroPrimo [] = []
filtroPrimo (x:xs) = [x] ++ (filtroPrimo (filter (\e -> not(mod e x == 0)) (xs)))

pertenceF :: [Int] -> Int -> Bool
pertenceF [] n = False
pertenceF (x:xs) n | x == n = True
                   | otherwise = pertenceF xs n

-- Dados dois pontos num espaço tridimensional, defina uma função distancia e um tipo Ponto de tal forma que a função calcule a distância entre dois pontos passados como parâmetros. A função tem tipo Ponto -> Ponto -> Double.

type PontoP = (Double, Double,Double)

distanciaP :: PontoP -> PontoP -> Double
distanciaP (x1,y1,z1) (x2,y2,z2) = sqrt(((x2 - x1)*(x2 - x1)) + ((y2 - y1)*(y2 - y1)) + ((z2 - z1)*(z2 - z1)))

-- Usando compreensão de lista, defina uma expressão que calcule a soma 12+22+...+1002 dos quadrados dos cem primeiros inteiros positivos

somaQ = sum [ x * x | x <- [1 .. 100]]

--Suponha que uma grade de coordenadas de tamanho m x n is dada por uma lista de todos os pares (x,y) tal que 0⩽x⩽m e 0⩽y⩽n. Usando compreensão de lista, defina a função grid:: Int -> Int -> [(Int, Int)] que retorna uma grade de um dado tamanho

grid :: Int -> Int -> [(Int,Int)]
grid x y = [ (x1,y1) | x1 <- [ 0 .. 1 ], y1 <- [ 0 .. y ]]

--Usando compreensão de lista e a função grid definida acima, defina a função square :: Int -> [(Int, Int)] que retorna as coordenadas do quadrado de tamanho n, excluindo a diagonal de (0,0) a (n,n).

square :: Int -> [(Int, Int)]
square n = [ x | x <- (grid n n), x /= (0,0), x /= (n,n)]

-- Defina a função recursiva merge :: Ord a => [a] -> [a] -> [a] que mescla duas listas ordenadas em uma única lista ordenada

merge :: Ord a => [a] -> [a] -> [a] 
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge ys (x:xs)

--Usando a função merge, defina a função msort :: Ord a => [a] -> [a] que implementa merge sort, no qual uma lista vazia e uma lista com um único elemento estão ordenadas; qualquer outra lista é ordenada por mesclar as duas listas que resultam de ordenar as duas metades da lista separadamente. 

--REVER

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve l = (halveAux ((length l) `div` 2) l, halveAux2 ((length l) `div` 2) l)

halveAux :: Int -> [a] -> [a]
halveAux 0 _ = []
halveAux n (x:xs) = [x] ++ halveAux (n-1) xs

halveAux2 :: Int -> [a] -> [a]
halveAux2 0 l1 = l1
halveAux2 n (x:xs) = halveAux2 (n-1) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge (msort(fst (halve l))) (msort(snd (halve l)))

{--[2,6,4,9,3,1]
[2,6,4] [9,3,1]
[2,6] [4]  [9,3] [1]
[2] [6]  [4]   [9] [3]  [1]
[2,6] [4]  [3,9] [1]
[2,4,6] [1,3,9]
[1,2,3,4,6,9]--}

-- Defina e implemente a função aplicaFuncoes :: [Int->Int] -> [Int] -> [[Int]] que recebe uma lista de funções unárias e uma lista de valores e retorna uma lista de listas na qual cada lista contém a lista de valores recebidos na entrada aplicada a uma das funções.

aplicaF :: [Int->Int] -> [Int] -> [Int]
aplicaF f x = [f1 x1 | f1 <- (f), x1 <- (x)]

dobra n = 2*n
triplica n = 3*n

-- Implemente um tipo algébrico DiasSemana, começando do Domingo e indo até Sábado, instancie ele para Enum, Show, Ord e Eq e crie 3 funções: 

data DiasSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Enum, Show, Ord, Eq)

-- Dada uma lista de dias da semana retorna os dias úteis ordenados;
ordenaUteis :: [DiasSemana] -> [DiasSemana]
ordenaUteis [] = []
ordenaUteis [x] = [x]
ordenaUteis l1 = merge (ordenaUteis (fst (divideLista l1))) (ordenaUteis (snd (divideLista l1)))

divideLista :: [DiasSemana] -> ([DiasSemana],[DiasSemana])
divideLista [] = ([],[])
divideLista l1 = (take (length l1 `div` 2) l1, drop (length l1 `div` 2) l1)


-- Dada uma lista de tuplas (Dia da semana, data) e um dia da semana, retorna uma lista com somente as datas que sejam do mesmo dia da semana;

datasIguais :: [(DiasSemana,Int)] -> DiasSemana -> [Int]
datasIguais [] ds2 = []
datasIguais ((ds,n):xs) ds2 | (ds == ds2) = n : datasIguais xs ds2
                            | otherwise = datasIguais xs ds2

-- Recebe um dia da semana (referente ao primerio dia do mes) e retorna uma lista de tuplas com todos os dias do mês. Considere o mês como tendo 30 dias.

imprimeMes :: DiasSemana -> [(Int,DiasSemana)]
imprimeMes ds = zip [ x | x <- [1 .. 30]] [toEnum(((x - 1) + fromEnum ds) `mod` 7) | x <- [1 .. 30]]

-- Exercícios - listas, tipos algébricos, funções de alta ordem

--Crie uma função de ordenação sort::(a->a->Bool)->[a]->[a] que recebe uma função de comparação e uma lista, e ordena a lista de acordo com a função. 

sort :: (a -> a -> Bool) -> [a] -> [a]
sort f [] = []
sort f (x:y:xs) | f x y == True = x : sort f y:xs
                | otherwise = sort f (xs++[x]++[y])


--ptthon -m http.server
-- Exercícios - listas e funções de alta ordem

-- 1 Usando compreensão de lista, defina uma função que, dado um inteiro positivo n, retorna uma lista com os fatoriais de 1 até n

fatC :: Int -> [Int]
fatC n = [ x * x | x <- [1 .. n]]

-- A função testaLista ::( a −> Bool) −> [a] −> Bool retorna True caso os elementos da lista dada como argumento satisfaçam a função passada como primeiro argumento; caso contrário, retorna False. Defina a função testaLista das seguintes maneiras:
--Usando recursão

testaLista :: (a -> Bool) -> [a] -> Bool
testaLista f [] = True
testaLista f (x:xs) = f x && testaLista f xs

--Usando map e and

testaLista2 :: (a -> Bool) -> [a] -> Bool
testaLista2 f l = and (map f l) 

--Usando foldr

testaLista3 :: (a -> Bool) -> [a] -> Bool
testaLista3 f l = foldr (&&) True (map f l) 

-- O sistema de arquivos de um sistema operacional é constituído de arquivos simples e diretórios (pastas). Um arquivo simples possui nome e conteúdo; um diretório possui um nome e uma lista de arquivos dentro dele.

-- 1

type Nome = String
type Conteudo = String

data Arquivo = Simples Nome Conteudo | Diretorio Nome [Arquivo] deriving (Show,Eq)

arq1 = Simples "Video" "Coisas legais"
arq2 = Diretorio "Pasta" [Simples "Arq1" "Descricao", Simples "Arq2" "Descricao"]

-- 2





------ Exercícios - Lista 19/04
-- https://piazza-resources.s3.amazonaws.com/jdaiwpsle464ey/jg6etq0fp0oio/listaHaskell_lab.pdf?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=ASIAIZ3B4FRTV5SFJRTA%2F20180419%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20180419T110856Z&X-Amz-Expires=10800&X-Amz-SignedHeaders=host&X-Amz-Security-Token=FQoDYXdzENP%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaDDbbRPqrKW5ckyESPyK3AzNB0j%2F0CfsHfefNQ6D1175flTPVhxuPXca9Qtd3AkxXAipo2TMfwgQzAKRtkQiz%2FCLqMPI1LiSaEgGnVrzqRWaFMjIQgUgZQmMq1ZZsnWUtLFCljfXJdlAiD0UuezCvMmA203vr5pkX9MfsNEae%2BM4c0OdwdhocDhKwMZmKWeuDDsVKfCmuxdKzBHaPhlHyUp36Pa0YLP1Ci41so3mKcIPIBevPC6E8gC7aL0UBWKf527BF8fYI0y9nYX5Vqg3oDvQiy1r1KK2pIy06nXAq9JFNr29Ggv6lQ5%2B%2FFHmqsqWSrIBB%2FnUUgkdalBI0bmRAOx%2Bvr%2BHe%2FuhkfXZ9DZoDHFTDzaEd9k70a3H1usiLyjjoAj3CrQPEWzEg4Y4U7Wq1WPAKgMJTgIRGYojLz900zzoBtbRPwCxFWg3585jZye1xyfK%2FdGEim3TsiWtue1O53u1YjQQpZxJMs3PtgQTGB03ahjwWXjkoSYi3bvJV2gYErXtbGrlHvGqK7O0dFVPgBjJjikBEzH0xOJ2FRq9qIp0fPKGfqYLC3vn7t5wXAZBX1Ne4mcgRErN%2FSMS0CWAIcgMdXAETXGkoy9Ph1gU%3D&X-Amz-Signature=3d6b35bb5cc4bfa18232628da8c491d2b0a22f4fb1a17f1b15c63e15c5ca0e9c

-- // 1
-- // a - sem comprenssao

f :: [Int] -> [Int]
f [] = []
f (x:xs) | xs /= [] && x == head xs = x : f xs
         | otherwise = f xs

-- // b - com comprenssao

--f2 :: [Int] -> [Int]
f2 (x:xs) = [ n1 | (n1,n2) <- (zip [ x | x <- (x:xs)] [ s | s <- (xs)]), n1 == n2]

-- // 2

g :: [Int] -> Bool
g [] = True
g l = foldr (&&) True $ map (\x -> x `mod` 2 == 0) $ filter (\x -> (x > 10 && x <= 100) == True) l

-- // 3

type NomeFabricante = String
type Watts = Int

data Lampada = Compacta NomeFabricante Watts | Incandescente NomeFabricante Watts deriving (Show, Eq)


lamp = (Compacta "Unos SA" 12)
lamp2 = (Incandescente "Unos SA" 15)

lampIguais :: Lampada -> Lampada -> Bool
lampIguais (Compacta n1 w1) (Compacta n2 w2) | n1 == n2 && w1 == w2 = True
                                          | otherwise = False   
lampIguais (Incandescente n1 w1) (Incandescente n2 w2) | n1 == n2 && w1 == w2 = True
                                                       | otherwise = False   

-- // 4
--a
data Lustre = Pendente Lampada | Barra Lustre Lustre deriving (Show)

lustre1 = Pendente (Compacta "Unos SA" 12)
lustre2 = Barra (Pendente (Compacta "Unos SA" 12)) (Pendente (Incandescente "Dales SA" 8))
lustre3 = Barra (Barra (Pendente (Compacta "Unos SA" 12)) (Pendente (Incandescente "Dales SA" 8))) (Pendente (Compacta "Kels SA" 20))
lustre4 = Barra (Barra (Pendente (Compacta "Unos SA" 12)) (Pendente (Incandescente "Dales SA" 8))) (Barra (Pendente (Compacta "Unos SA" 12)) (Barra (Pendente (Compacta "Unos SA" 4)) (Pendente (Incandescente "Dales SA" 4))))

--b

potenciaLustre :: Lustre -> Int
potenciaLustre (Pendente (Compacta _ w)) = w
potenciaLustre (Pendente (Incandescente _ w)) = w
potenciaLustre (Barra l1 l2) = potenciaLustre l1 + potenciaLustre l2 


--c

bal :: Lustre -> Bool
bal (Pendente l1) = True
bal (Barra l1 l2) = potenciaLustre (l1) == potenciaLustre (l2) 






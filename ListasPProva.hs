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

-- Exercícios - listas e funções de alta ordem

-- 1 Usando compreensão de lista, defina uma função que, dado um inteiro positivo n, retorna uma lista com os fatoriais de 1 até n

fatC :: Int -> [Int]
fatC n = [ x * x | x <- [1 .. n]]

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




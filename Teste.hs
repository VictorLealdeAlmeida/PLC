import Data.Char

soma 1 = 1
soma n = soma(n-1) + n

fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial(n - 1)

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

guarda x |(x==0)=0
         |(x==1)=1
         |otherwise=10

my_and :: Bool -> Bool -> Bool
my_and False _ = False
my_and _ False = False
my_and True True = True

func :: (Int, Int) -> (Int, Int) -> (Int, Int)
func (a,b) (c,d) = (a+c, b+d)

nomes :: (String, String, String)
nomes = ("Victor", "Leal", "Porto")

selec_prim (x, _, _) = x
selec_sec (_, y, _) = y
selec_ter (_, _, z) = z

type Nome = String
type Idade = Int
type Linguagem = String
type Pessoa = (Nome, Idade, Linguagem)

pessoa :: Pessoa
pessoa = ("Joao", 20, "Haskell")

my_fst :: Pessoa -> Nome
my_fst (n, i, l) = n

size_list [] = 0
size_list (x:xs) = 1 + size_list xs

eh_igual :: [Int] -> [Int] -> Bool
eh_igual [] [] = True 
eh_igual [] _ = False 
eh_igual _ [] = False 

eh_igual (x:xs) (y:ys)  | (x == y) = eh_igual xs ys
                        | otherwise = False

inv_aux :: [t] -> [t] -> [t]
inv_aux [] l_inv = l_inv
inv_aux (x:xs) l_inv = (inv_aux xs l_inv)++[x]

inv_lista :: [t] -> [t]
inv_lista [] = []
inv_lista (x:xs) = inv_lista xs ++ [x]


pertence :: [Int] -> Int -> Bool 
pertence [] _ = False
pertence (x:xs) n   | (n==x) = True
                    | otherwise = pertence xs n

maior :: [Int] -> Int
maior [x] = x
maior (x:xs)| (x > maior xs) = x


todos_pares :: [Int] -> Bool
todos_pares [] = True
todos_pares (x:xs)  | (mod x 2 /= 0) = False
                    | otherwise = todos_pares xs



--zip :: [a] -> [b] -> [(a,b)]

--[1,2,3]
--['a','b','c']

--zip [1,2,3] ['a','b','c']

--SORT

lista :: [Int]
lista = [5,1,10,3,9]


get_menor :: [Int] -> Int
get_menor [x] = x
get_menor (x:xs)| (x < get_menor xs) = x
                | otherwise = get_menor xs
 
remove_menor :: [Int] -> [Int]
remove_menor [] = []
remove_menor (x:xs) | (x == get_menor (x:xs)) = xs
                    | otherwise = (x:remove_menor xs)

aux_ordena :: [Int] -> [Int] -> [Int]
aux_ordena lista_ordenada [] = lista_ordenada
aux_ordena lista_ordenada (x:xs) = aux_ordena (lista_ordenada++[get_menor (x:xs)]) (remove_menor (x:xs))


ordena :: [Int] -> [Int]
ordena [] = []
ordena lista = aux_ordena [] lista

--SORT

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs)++[x]


--Questao da Aula 3

imprimeTabela :: Int -> String
imprimeTabela n = imprimeSemanas n
                  ++ " Total: " ++ convert (imprimeTotal n)
                  ++ " Media: " ++ convert (imprimeMedia n)


semana :: Int -> Int
semana 0 = 12
semana 1 = 14
semana 2 = 15


imprimeSemanas :: Int -> String
imprimeSemanas 0 = "Semana 0"
imprimeSemanas n = ("Semana " ++ show n) ++ " " ++ imprimeSemanas (n-1) 

imprimeTotal :: Int -> Int
imprimeTotal 0 = 12
imprimeTotal n = semana n + imprimeTotal (n-1)

imprimeMedia :: Int -> Int
imprimeMedia 0 = 12
imprimeMedia n = (div (imprimeTotal n) (n+1))


convert :: Int -> String
convert n = show n

---

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (get_menor [a,b,c], get_maior [a,b,c]) 


get_maior :: [Int] -> Int
get_maior [x] = x
get_maior (x:xs) | x > (get_maior xs) = x
                 | otherwise = get_maior xs


--------

type Ponto = (Float, Float)
p1 :: Ponto
p1 = (4,5)

p2 :: Ponto
p2 = (4,3)

eh_vertical :: Ponto -> Ponto -> Bool
eh_vertical (x,y) (w,z) | x == w = True
                        | otherwise = False





------ Aula 5

membroLista :: [Int] -> Int -> Bool 
membroLista [] _ = False
membroLista (x:xs) n   | (n==x) = True
                       | otherwise = pertence xs n


digitoLista :: [Char] -> [Char] 
digitoLista [] = []
digitoLista (x:xs) 
    | isDigit x = x : digitoLista xs
    | otherwise = digitoLista xs


somarParesLista :: [(Int,Int)] -> [Int]
somarParesLista [] = []
somarParesLista ((x1,x2):xs) = x1+x2 : somarParesLista xs



--membro :: [Int] -> Int -> Bool

















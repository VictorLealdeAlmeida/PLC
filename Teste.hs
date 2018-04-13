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




------ Aula Tipos Algebricos

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

expr1 = (Lit 4)
expr2 = (Add (Lit 2) (Sub (Lit 4) (Lit 2)))

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2) 
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"

data List t = NilL | Cons t (List t) deriving Show

list1 = Cons 7 (Cons 3 (Cons 2 (NilL)))

toList :: List t -> [t]
toList (NilL) = []
toList (Cons t xs) = t : toList xs

fromList :: [t] -> List t
fromList [] = NilL
fromList (x:xs) = (Cons x(fromList xs)) 


data Tree t = Nil | Node (Tree t) t (Tree t) deriving Show

tree1 = Node (Nil) 4 (Nil)
tree2 = Node (Node (Nil) 7 (Nil)) 4 (Node (Nil) 3 (Nil))
tree3 = Node (Node (Node (Nil) 4 (Nil)) 7 (Node (Nil) 5 (Nil))) 1 (Node (Nil) 3 (Nil))

depth :: Tree t -> Int
depth (Nil) = 0
depth (Node t1 value t2) = 1 + max (depth t1) (depth t2)

callapse :: Tree t -> [t]
callapse (Nil) = []
callapse (Node t1 value t2) = [value] ++ callapse t1 ++ callapse t2

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f Nil = Nil
mapTree f (Node t1 value t2) = (Node (mapTree f t1) (f value) (mapTree f t2))

dobra n = 2 * n

------ Exercícios - listas e tipos algébricos

--Defina uma função que, dados dois números x e y, retorne como resultado o m.d.c. de x e y.

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

somaQ = sum [ x * x | x <- [0 .. 99]]


-- Exercícios - listas e funções de alta ordem

-- 1 Usando compreensão de lista, defina uma função que, dado um inteiro positivo n, retorna uma lista com os fatoriais de 1 até n

fatC :: Int -> [Int]
fatC n = [x*x | x <- [1 .. n]]










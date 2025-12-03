
--1)O fatorial de um número natural n (indicado por n!) é o produto de todos os números inteiros de 1 até n. Faça uma função que calcule o fatorial de um dado número n.
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

--2)A sequência de Fibonacci começa com 0 e 1, e cada número seguinte é a soma dos dois anteriores:
--F(0) = 0
--F(1) = 1
--F(n) = F(n - 1) + F(n - 2)
--Defina a função fibonacci em Haskell, que dado um valor n, retorne o elemento correspondente da sequência.
--Exemplo:fibonacci 6 -> 8
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--3) Implemente a função inverte :: [a] -> [a] que inverta os elementos de uma lista sem usar reverse.
inverte :: [a] -> [a]
inverte [] = []
inverte (z:zs) = inverte zs ++ [z]

--4)Crie a função maximo :: Ord a => [a] -> a que encontre o maior elemento de uma lista usando recursão.
--Exemplo: maximo [1,5,3,6] -> 6
maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs)
  | x >= maxResto = x      
  | otherwise     = maxResto
  where maxResto = maximo xs

--5)Defina a função nrepetidos que dada uma lista retorna uma lista de tuplos com os elementos repetidos e o número de ocorrencias desses elementos.
--ex:nrepetidos “oratoroiacorda''-> [('o', 4),('r',3),('a',3)]
--   nrepetidos “ola'' -> []
nrepetidos :: Eq a => [a] -> [(a, Int)]
nrepetidos [] = []
nrepetidos (x:xs)
  | cont > 1 = (x, cont) : nrepetidos resto
  | otherwise = nrepetidos resto
  where
    cont  = contar x (x:xs)
    resto = remover x xs

contar :: Eq a => a -> [a] -> Int
contar _ [] = 0
contar x (y:ys)
  | x == y    = 1 + contar x ys
  | otherwise = contar x ys

remover :: Eq a => a -> [a] -> [a]
remover _ [] = []
remover x (y:ys)
  | x == y    = remover x ys
  | otherwise = y : remover x ys

--6)Defina a função contaQuantosDiferentes:: [Int] -> Int-> Int que retorna o número de elementos diferentes do inteiro na lista.
--exemplo: contaQuantosDiferentes [1,3,4,1] 1 -> 2
--         contaQuantosDiferentes [1,3,4,1] 2 -> 4
contaQuantosDiferentes :: [Int] -> Int -> Int
contaQuantosDiferentes lista num = 
  length (filter (\x -> x /= num) lista )

--7) Faça uma solução para, dada uma lista de inteiros, retornar uma dupla de listas de inteiros onde a primeira conterá os elementos ímpares e a segunda os elementos
--pares passados como parâmetro.
--Exemplo:separa [1,4,3,4,6,7,9,10] -> ([1,3,7,9],[4,4,6,10])
separa :: [Int] -> ([Int], [Int])
separa [] = ([], [])
separa (x:xs)
  | x `mod` 2 == 0 = (impares, x : pares) 
  | otherwise      = (x : impares, pares)  
  where
    (impares, pares) = separa xs

--8)Faça uma solução para o seguinte problema: Dada uma String e um caractere a, retorne quantos caracteres da lista são iguais a a.
--Exemplo:conta "ABCAABCDDA" "B" -> 2
conta :: String -> Char -> Int
conta [] _ = 0                      -- caso base: string vazia → 0 ocorrências
conta (x:xs) c
  | x == c    = 1 + conta xs c      -- se o caractere atual for igual, soma 1
  | otherwise = conta xs c          -- senão, apenas continua

--9)Para uma lista de elementos inteiros ordenada qualquer, faça uma função que retorne uma lista de inteiros ordenada sem elementos repetidos.
--Exemplo: purifica [1,1,4,5,5,5,6,7,8,8] -> [1,4,5,6,7,8]
purificaOrdenada :: [Int] -> [Int]
purificaOrdenada [] = []
purificaOrdenada [x] = [x]  --Caso base
-- Para o caso geral (x:y:ys), compare o elemento atual (x) com o próximo (y).Se forem iguais, descartamos x e continuamos com y.
purificaOrdenada (x:y:ys)
  | x == y    = purificaOrdenada (y:ys) 
  | otherwise = x : purificaOrdenada (y:ys) 

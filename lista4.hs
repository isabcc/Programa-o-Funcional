--1)Crie uma função cabeca :: [a] -> a que retorne o primeiro elemento de uma lista.
--Exemplo:cabeca [10, 20, 30] → 10
cabeca :: [a] -> a
cabeca (x:xs) = x

--2)Crie uma função cauda :: [a] -> [a] que retorne todos os elementos de uma lista, exceto o primeiro.
--Exemplo:cauda [10, 20, 30] → [20, 30]
cauda :: [a] -> [a]
cauda (x:xs) = xs

--3)Crie uma função situacaoAluno :: (Float, Float) -> String que receba duas notas e retorne a situação do aluno:
-- “Aprovado” se média ≥ 6
-- “Recuperação” se média ≥ 4
-- “Reprovado” caso contrário
--Exemplo:situacaoAluno (6.5, 5.5) → "Recuperação
situacaoAluno :: (Float, Float) -> String
situacaoAluno (nota1, nota2)
  | media >= 6.0 = "Aprovado"
  | media >= 4.0 = "Recuperação"
  | otherwise    = "Reprovado"
  where media = (nota1 + nota2) / 2.0

--4)Implemente a função tamanho :: [a] -> Int que conte o número de elementos de uma lista. (Sem usar length)
--Exemplo:tamanho [1,2,3,4] → 4
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--5)Crie uma função somaPares :: [Int] -> Int que some apenas os números pares de uma lista.
--Exemplo:somaPares [1,2,3,4,5,6] → 12
somaPares :: [Int] -> Int
somaPares lista = sum (filter ehPar lista)
  where
    ehPar n = n `mod` 2 == 0

--6)Crie uma função descricaoLista :: [a] -> String que descreva uma lista:
-- ● “Lista vazia” se não tiver elementos
-- ● “Um elemento” se tiver só um item
-- ● “Vários elementos” caso contrário
--Exemplo:descricaoLista [1,2,3] → "Vários elementos"
descricaoLista :: [a] -> String
descricaoLista [] = "Lista vazia"
descricaoLista [x] = "Um elemento"
descricaoLista _ = "Vários elementos"

--7)Crie uma função tipoTriangulo :: (Eq a, Num a) => (a, a, a) -> String que determine o tipo de um triângulo, dados três lados em uma tupla:
-- ● “Equilátero” se todos os lados forem iguais
-- ● “Isósceles” se dois lados forem iguais
-- ● “Escaleno” se todos forem diferentes
--Exemplo:tipoTriangulo (3,3,3) → "Equilátero"
tipoTriangulo :: (Eq a, Num a) => (a, a, a) -> String
tipoTriangulo (a, b, c)
  | a == b && b == c = "Equilátero"  
  | a == b || a == c || b == c = "Isósceles" 
  | otherwise = "Escaleno"

--8)Crie uma função contaVogais :: String -> Int que conte quantas vogais existem em uma palavra.
--Considere apenas as letras minúsculas ‘a’, ‘e’, ‘i’, ‘o’, ‘u’.
--Exemplo:contaVogais "haskell" → 2
contaVogais :: String -> Int
contaVogais palavra = length (filter isVowel palavra)
  where
    vogais = ['a', 'e', 'i', 'o', 'u']
    isVowel char = char `elem` vogais

--9)Crie uma função produtoEscalar :: [Int] -> [Int] -> Int que calcule o produto escalar entre dois vetores (listas de inteiros).
--Dica: multiplique os elementos nas mesmas posições e some o resultado.
--Exemplo:produtoEscalar [1,2,3] [4,5,6] → 32
produtoEscalarRecursivo :: [Int] -> [Int] -> Int
produtoEscalarRecursivo [] _  = 0
produtoEscalarRecursivo _ []  = 0
produtoEscalarRecursivo (x:xs) (y:ys) = (x * y) + produtoEscalarRecursivo xs ys

--10)Crie uma função maiorElemento :: (Ord a) => [a] -> a que retorne o maior elemento de uma lista.Use casamento de padrões e guardas (sem usar maximum).
--Exemplo:maiorElemento [3,8,2,9,5] → 9
maiorElemento :: (Ord a) => [a] -> a
maiorElemento [x] = x
maiorElemento (x:xs)
  | x >= maiorResto = x
  | otherwise       = maiorResto
  where maiorResto = maiorElemento xs
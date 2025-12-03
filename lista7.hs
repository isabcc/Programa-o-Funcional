-- 1. Mostre como a seguinte list comprehension [f x | x <- xs, p x] pode ser reescrita
--utilizando funções de alta ordem como map e filter. Tente entender e aplicar o  seguinte exemplo:
-- [(+7) x | x <- [1..10], odd x]
questao1 = map (+7) (filter odd [1..10])

-- 2. Faça uma solução em Haskell que, dada uma lista de inteiros, ela retorne uma lista
-- com uma repetição de cada elemento de acordo com seu valor.
-- Exemplo: proliferaInt [3,0,2,4,0,1] = [3,3,3,2,2,4,4,4,4,1]
proliferaInt :: [Int] -> [Int]
proliferaInt xs = concatMap (\x -> replicate x x) (filter (>0) xs)

--3. Defina a função dec2int :: [Int] -> Int que converta uma lista de inteiros para um inteiro.
--Exemplo:[2,3,4,5] = 2345
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- 4 - Defina a função evenCubes :: Int -> [Int] que, dado um limite, retorne a lista do cubo
--dos nú pares até o limite fornecido. Exemplo: evenCubes 10 = [8, 64, 216, 512]
evenCubes :: Int -> [Int]
evenCubes n = map (^3) (filter even [1..n-1])

--5.Faça em Haskell uma solução para, dada uma lista de inteiros, retornar uma dupla de listas de inteiros onde a primeira conterá os elementos ímpares e a segunda os
--elementos pares passados como parâmetro.
--Exemplo: separa [1,4,3,4,6,7,9,10] = ([1,3,7,9],[4,4,6,10])
separa :: [Int] -> ([Int], [Int])
separa xs = (filter (\x -> x `mod` 2 /= 0) xs, filter (\x -> x `mod` 2 == 0) xs)

-- 6- Faça um programa que receba duas listas Int A e B e retorne a união dessas duas
--listas. Obs: No retorno da função não deve haver elementos repetidos, por exemplo.
--Exemplo: uniao [1,2,3,4] [1,2,9] = [3,4,1,2,9].
uniao :: [Int] -> [Int] -> [Int]
uniao a b = a ++ filter (\x -> not (x `elem` a)) b

--7. Dada uma lista de caracteres [Char], e um caractere a, retornar quantos caracteres da lista são iguais a a.
--Exemplo: conta "ABCAABCDDA" "B" = 2
conta :: [Char] -> Char -> Int
conta lista a = length (filter (\x -> x == a) lista)

-- 8 -Faça uma função que retorne qual é a moda de uma lista, ou seja, o elemento que
--mais repete (Caso tenha mais de uma moda, retorne todas).
--Exemplo:
--moda [1,2,3,4,5,5,6,7,8,10] = [6]
--moda [1,1,2,2,3,3,4,4,5,5,5,6,6,7,7,7,8,8,9,10,10,10] = [5,7,10]

moda :: Eq a => [a] -> [a]
moda [] = []
moda xs = 
  let unicos = removeRepetidos xs
      contagens = map (\x -> (x, conta x xs)) unicos
      maxFreq = maximum $ map snd contagens
  in map fst $ filter (\(_,c) -> c == maxFreq) contagens
  where
    conta x = length . filter (== x)
    removeRepetidos [] = []
    removeRepetidos (x:xs) = x : removeRepetidos (filter (/= x) xs)

--9
base :: Int -> (Int, String, String, Char)
base x
    | x == 0 = (1793, "Pedro Paulo", "MESTRE", 'M')
    | x == 1 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
    | x == 2 = (1534, "João de Medeiros", "DOUTOR", 'M')
    | x == 3 = (1267, "Cláudio César de Sá", "DOUTOR", 'M')
    | x == 4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
    | x == 5 = (1888, "Rita de Matos", "MESTRE", 'F')
    | x == 6 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
    | otherwise = (0, "", "", '0')

professores :: [(Int, String, String, Char)]
professores = map base [0..6]

-- (a)  O número de doutores na base.
numDoutores :: Int
numDoutores = length (filter (\(_,_,titulo,_) -> titulo == "DOUTOR") professores)

-- (b) O número de mulheres.
numMulheres :: Int
numMulheres = length (filter (\(_,_,_,sexo) -> sexo == 'F') professores)

-- (c) O número de mestres do sexo masculino.
mestresMasculinos :: Int
mestresMasculinos = length (filter (\(_,_,titulo,sexo) -> titulo == "MESTRE" && sexo == 'M') professores)

-- (d) O nome do professor mais antigo (número de menor matrícula)
maisAntigo :: String
maisAntigo = pegaNome (foldl menor (head professores) (tail professores))
  where
    menor (m1,n1,t1,s1) (m2,n2,t2,s2)
      | m1 < m2   = (m1,n1,t1,s1)
      | otherwise = (m2,n2,t2,s2)

    pegaNome (_, nome, _, _) = nome


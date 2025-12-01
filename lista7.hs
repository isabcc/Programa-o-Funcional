--Isadora Resende Grandeaux e Gabriella Caproni

--3. Defina a função dec2int :: [Int] -> Int que converta uma lista de inteiros para um inteiro.
--Exemplo:[2,3,4,5] = 2345
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

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


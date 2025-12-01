--1)Implemente a função contaImpares :: [Int] -> Int que retorna a quantidade de números ímpares presentes em uma lista. Restrição: Use apenas recursão explícita e casamento de padrões.
contaImpares :: [Int] -> Int
contaImpares [] = 0
contaImpares (x:xs) 
  | mod x 2 /= 0 = 1 + contaImpares xs
  | otherwise = 0 + contaImpares xs

--2)Defina a função somaQuadradosPares :: [Int] -> Int que recebe uma lista de números inteiros e:
--1. Filtra os números pares.
--2. Eleva cada número par ao quadrado.
--3. Soma os resultados obtidos.
--Restrição: Use obrigatoriamente as funções de alta ordem map, filter e foldr.
somaQuadradosPares :: [Int] -> Int
somaQuadradosPares lista =
    foldr (+) 0                                          
        (map (\x -> x * x)                              
            (filter (\x -> mod x 2 == 0) lista)        
        )
--3 - Considere o tipo de dado para meses: data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez.
-- a) Derivação: Defina a cláusula deriving necessária para que seja possível usar o tipo Mes como um índice em enumerações e na função succ (sucessor).
-- b) Função: Defina a função proximoMes :: Mes -> Mes que retorna o mês seguinte.
--Para o mês de Dez, deve retornar Jan. Restrição: Use a função succ e a funçãomaxBound (limite máximo) da classe de tipo derivada.

--4 - Qual é o resultado da seguinte expressão:filter (\x -> x > 10) (map (\y -> 2*y + 1) (foldr (:) [] [1..5])) ?
--Resultado da Etapa 1: [1, 2, 3, 4, 5]
--Resultado da Etapa 2: [3, 5, 7, 9, 11]
--Resultado Final: [11]

--5 - Explique o que acontece ao tentar calcular 10 + 'a' no compilador interativo. Qual erro de tipo é gerado? Por que esse erro não ocorre em expressões como 1 + 2 ou 'a' + 'b'?
--Aparece um erro de instancia, que indica que não conseguiu achar correspondencia entre uma variavel do tipo char e do tipo Num, isso não acontece nos outtros exemplos porque eles são do mesmo tipo
-- o que garante corrensondencia no processo de soma

--6 - Haskell utiliza o conceito de Currying. Descreva o que é o Currying no contexto de funções com múltiplos argumentos.
--R:Currying é a técnica fundamental em Haskell onde uma função que parece aceitar múltiplos argumentos é, na verdade, transformada em uma sequência de funções que aceitam um único argumento de cada vez.

--7 - Usando a assinatura mult :: Int -> Int -> Int, explique como o compilador de Haskell "vê" essa função e o que é retornado na primeira aplicação, por exemplo, em mult 5.
--O compilador interpreta como mult Int->(Int -> Int), dde forma que ele aceita o primeiro argumento e retorna uma nova funçãoe e essa nova função ( entre parenteses) espera o segundo argumento para so daí produzir e retornar o resltado. Agoraem relação ao exemplo,
-- em primeira instância a função mult 5 retorna 10 e o resultado final é 50.

--8 - Mostre o passo-a-passo da avaliação (redução) da seguinte expressão e indique o resultado final:
--foldr (++) [] (map (\x -> if x `mod` 2 == 0 then [show x] else []) [1..6])
--Primeiramente, é criado uma lista de numeros de 1 a 6, depois é definido que se um número for ímpar será atribuído uma lista vazia e se for par 
--ele será convertido em string e será atribuido como lista,isso através da função map, depois disso a função foldr (++) [], irá juntar tudo isso em uma coisa só
--de forma que as listas vazias sejam "ignoradas" e as listas com os números pares em forma de string, virem uma string só, resultando em "246"

--9 - Analise função funcMisteriosa. O que essa função faz? Dê um exemplo de como ela pode ser utilizada.
--funcMisteriosa :: (a -> a) -> [a] -> [a]
--funcMisteriosa f [] = []
--funcMisteriosa f (x:xs) = f (f x) : funcMisteriosa f xs
--A função funcMisteriosa é uma implementação recursiva de mapeamento que aplica uma função de transformação (f) duas vezes a cada elemento de uma lista, mantendo a estrutura da lista original. Sua assinatura (a -> a) -> [a] -> [a] indica que ela recebe a função f e 
--uma lista, retornando uma nova lista com a mesma quantidade de elementos, onde cada x foi transformado em f (f x). Por exemplo, se f for a função que soma 1 (\x -> x + 1), ao aplicar funcMisteriosa f [10, 20], o resultado será [12, 22], 
--pois cada elemento foi incrementado duas vezes.

--10 - Implemente a função funcSecreta utilizando funções de Alta Ordem sem usar recursão explícita:
--funcSecreta :: [Int] -> [Int]
--funcSecreta [] = []
--funcSecreta (x:xs) = (x + 5) : funcSecreta xs
funcSecreta :: [Int] -> [Int]
funcSecreta = map (\x -> x + 5)

--11)Explique, em termos conceituais, a principal diferença entre definir um tipo usando type (como type ID = Int) e usando data (como data ID = ID Int).
--A diferença principal entre type e data em Haskell é que type cria apenas um sinônimo ou apelido para um tipo existente (ex: type ID = Int), tratado pelo compilador como o mesmo tipo original para fins de compatibilidade, sendo usado primariamente para melhorar a legibilidade.
-- Já o data cria um tipo de dado novo e distinto (ex: data ID = ID Int), que encapsula o tipo subjacente e exige o uso de um construtor de valor (o segundo ID) para empacotar ou desempacotar o dado, garantindo maior segurança de tipos ao impedir a mistura acidental de valores de tipos diferentes.
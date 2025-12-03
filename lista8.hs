--1)Você foi contratado para programar o sistema inteligente do elevador de uma grande empresa. Existem 4 níveis hierárquicos de funcionários. O sistema precisa tomar duas
--decisões diferentes:
--1. Liberar o andar: Um funcionário só pode ir para andares do seu nível ou inferiores.
--2. Modo "Reunião de Time": O sistema precisa verificar se dois funcionários são exatamente do mesmo cargo para autorizar uma reunião privada.
--Assim:
--1. Crie um tipo de dado chamado Nivel com os cargos: Estagiario, Junior, Pleno e Senior.
-- ○ Atenção: A ordem importa! O Senior manda mais que o Pleno, e assim por diante.
--2. Crie a função podeAcessar. Ela deve receber o nível do crachá e o nível do andar. Retorne True se o crachá for maior ou igual ao andar.
--3. Crie a função saoDoMesmoTime. Ela deve receber dois níveis e retornar True apenas se eles forem exatamente iguais.
--Ex:
-- > podeAcessar Senior Junior
-- > True
-- > saoDoMesmoTime Senior Junior
-- > False
data Nivel = Estagiario 
           | Junior 
           | Pleno 
           | Senior
           deriving (Show, Eq, Ord)

podeAcessar :: Nivel -> Nivel -> Bool
podeAcessar nivel_cracha nivel_andar = nivel_cracha >= nivel_andar

saoDoMesmoTime :: Nivel -> Nivel -> Bool
saoDoMesmoTime nivel1 nivel2 = nivel1 == nivel2

--2. Você está desenvolvendo um software de arquitetura. O sistema precisa lidar
-- com doistipos de formas: Círculos (definidos pelo raio) e Retângulos (definidos
-- pela largura e altura).
--  Crie o tipo algébrico Forma.
--  Crie uma função area que recebe uma Forma e calcula a sua área. 

data Forma = Circulo Float | Retangulo Float Float deriving (Show)
area :: Forma -> Float
area (Circulo r) = pi * r^2
area (Retangulo l a) = l * a


--3. Crie a função acessoPermitido que recebe a classificação do filme e a classificação da idade da pessoa. A função deve retornar True caso o filme seja menor ou igual à idade da pessoa.
data Classificacao = Livre
                   | DezAnos
                   | DozeAnos
                   | QuatorzeAnos
                   | DezoitoAnos
    deriving (Show, Eq, Ord)

acessoPermitido :: Classificacao -> Classificacao -> Bool
acessoPermitido classificacaoFilme idadePessoa = 
    classificacaoFilme <= idadePessoa

--4)Crie a função apenasUrgentes. Ela deve receber uma lista de Prioridades e retornar uma nova lista contendo apenas os itens maiores que Baixa.
data Prioridade = Baixa
                | Media
                | Alta
    deriving (Show, Eq, Ord)

apenasUrgentes :: [Prioridade] -> [Prioridade]

apenasUrgentes lista = filter (\p -> p > Baixa) lista

--5). Em um jogo, o personagem principal lança uma sequência de ataques. Precisamos calcular o dano total causado.
-- ● Crie a função calcularDanoTotal que recebe uma lista de [Ataque] e retorna um Int com a soma total.
-- ● Crie o tipo Ataque com os construtores Soco (dano 10) e Chute (dano 20).
--Exemplo:calcularDanoTotal [Soco, Soco, Chute] = 40
--calcularDanoTotal [ ] = 0
data Ataque = Soco  -- Dano de 10
            | Chute -- Dano de 20
            deriving (Show, Eq)
        
calcularDanoTotal :: [Ataque] -> Int
calcularDanoTotal [] = 0
calcularDanoTotal (Soco : xs) = 10 + calcularDanoTotal xs
calcularDanoTotal (Chute : xs) = 20 + calcularDanoTotal xs

--6) Utilizando o exemplo anterior, crie a função foiFatality, que verifica se o dano total foi maior ou igual a 100. Caso tenha sido, retorne “Game over”, caso contrário, “You’re alive!”
--Exemplo:foiFatality = “Game Over”
-- Definição do tipo Ataque e calcularDanoTotal (Requisito do exemplo anterior)
data Atacar = Socos  -- Dano de 10
            | Chutes -- Dano de 20
            deriving (Show, Eq)

calculoDanoTotal :: [Atacar] -> Int
calculoDanoTotal [] = 0
calculoDanoTotal (Socos : xs) = 10 + calculoDanoTotal xs
calculoDanoTotal (Chutes : xs) = 20 + calculoDanoTotal xs

foiFatality :: [Atacar] -> String
foiFatality ataques
  | danoTotal >= 100 = "Game over"
  | otherwise        = "You're alive!"
  where
    danoTotal = calculoDanoTotal ataques

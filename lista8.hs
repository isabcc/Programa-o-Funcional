--Isadora Resende Grandeaux e Gabriella Caproni

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
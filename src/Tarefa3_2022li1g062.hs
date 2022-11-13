{- |
Module      : Tarefa3_2022li1g062
Description : Movimentação do personagem e obstáculos
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g062 where

import LI12223

{-
1. Numa estrada ou rio com velocidade v, os obst ́aculos devem mover-se
|v| unidades na direc ̧c ̃ao determinada.
2. As jogadas Move Cima, Move Baixo, etc. fazem com que o jogador se
mova 1 unidade para cima, baixo, etc, respectivamente.
3. Mesmo quando o jogador n ̃ao efectua qualquer movimento (i.e. a sua
jogada  ́e Parado), se o personagem se encontrar em cima de um tronco,
o jogador acompanha o movimento tronco.
4. O jogador n ̃ao consegue escapar do mapa atrav ́es dos seus movimentos.
Por exemplo, se o jogador se encontrar na linha de topo do mapa,
ent ̃ao mover-se para cima n ̃ao tem qualquer efeito, uma vez que j ́a se
encontra no limite do mapa.
7
5. Ao deslocar os obst ́aculos de uma linha, lembre-se que estes, assim que
desaparecerem por um dos lados do mapa, devem reaparecer no lado
oposto.
6. O efeito de deslize do mapa n ̃ao  ́e para ser implementado nesta fun ̧c ̃ao.
Por outras palavras, as dimens ̃oes do mapa n ̃ao devem sofrer altera ̧c ̃oes
ap ́os invocar esta fun ̧c ̃ao

-}

{-
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo = animaJogo1 && animaJogo2 && animaJogo3 -}

--Exercício 1 / Exerćicio 5

animaJogo1 :: Jogo -> Jogada -> Jogo
animaJogo1 (Jogo j1 (Mapa l [])) j2 = []
animaJogo1 (Jogo j1 (Mapa l (Rio v, o):t)) j2 = ( Jogo j1 (Mapa l (aux3_1( (Rio v o):(aux3_1 t))) ) )  

aux3_1 :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
aux3_1 [] = []
aux3_1 ((Rio v, o):t)
   | v > 0 = aux3_1 (Rio v, ((drop v o) ++ (take v o)))++(aux3_1 t)
   | v < 0 = aux3_1 (Rio v, ((drop (-v) o) ++ (take (-v) o)))++(aux3_1 t)

--Exercício 2

{- 
animaJogo2 :: Jogo -> Jogada -> Jogo
animaJogo2 (Jogo (Jogador (x,y)) m) Parado = Jogo (Jogador (x,y)) m 
animaJogo2 (Jogo (Jogador (x,y)) m) (Move d) 
   |d == Cima == (Jogo (Jogador (x,y+1)) m) 
   |d == Baixo == (Jogo (Jogador (x,y-1)) m)   -}


--Exercício 3
{-
animaJogo3 :: Jogo -> Jogada -> Jogobn  
animaJogo3 (Jogo (x,y) (Mapa l (Rio v, o))) Parado = Jogo (x,y) m 
-}

--Exercício 4 (ñ percebo)


--Exercício 6
{-
animaJogo6 :: Jogo -> Jogada -> Jogo
animaJogo6 (Jogo j1 (Mapa l (Relva v, o):t)) 
-}



{- |
Module      : Tarefa3_2022li1g062
Description : Movimentação do personagem e obstáculos
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/2023.
-}
module Tarefa3_2022li1g062 where

import LI12223

import Test.HUnit 
{-
{- -- | Definição base de um jogo.
data Jogo =
  Jogo
    Jogador -- ^ o personagem do jogo
    Mapa -- ^ o mapa em que se está a jogar
  deriving (Show, Read, Eq)

-- | As acções que podem ser tomadas pelo 'Jogador' em cada estado do 'Jogo'.
data Jogada
  = Parado -- ^ tipo que define a ausência de uma acção do 'Jogador'
  | Move Direcao -- ^ um movimento do jogador numa determinada 'Direcao'
  deriving (Show, Read, Eq)

-- | Direção de uma 'Jogada' feita por um 'Jogador' no 'Mapa'.
data Direcao
  = Cima
  | Baixo
  | Esquerda
  | Direita
  deriving (Show, Read, Eq)

-- | O Mapa que constituí o 'Jogo'.
data Mapa =
  Mapa Largura [(Terreno, [Obstaculo])]
  deriving (Show, Read, Eq)

-}

data Direcao = Cima
| Baixo
| Esquerda
| Direita
data Jogada = Parado
| Move Direcao 

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador(a,b)) m@(Mapa tam x)) jog = Joao (moveJogador (Jogador (a,b)) jog m) ((Mapa tem (andaTudo x)))

moveJogador :: Jogador -> Jogada -> Mapa -> Jogador 
moveJogador (Jogador (a,b)) jog m@(Mapa tamanho c@((terr,obs):t))  | jog == Move Cima && (b /= (length (c)-1)) && verificaCimaLado (a,b) m (length(c)-b-1) a = Jogador (a,b+1)
                                                                   | jog == Move Baixo && b > 0 && verificaBaixo (a,b) m (length(c)-b) a = Jogador (a,b-1)
                                                                   | jog == Move Esquerda && a>0 && verificaCimaLado (a,b) m (length(c)-b) (a-1) = Jogador (a-1,b)
                                                                   | jog == Move Direita && (a<length (obs)-1) && verificaCimaLado (a,b) (m) (length(c)-b) (a+1) = Jogador (a+1,b)
                                                                   | otherwise = Jogador (a,b)

verificaCimaLado :: Coordenadas -> Mapa -> Int -> Int -> Bool 
verificaCimaLado (a,b) (Mapa tamanho ((terr,c:d):t)) 0 0 = False 
verificaCimaLado (a,b) (Mapa tamanho ((Estrada x2,c:d):t)) 1 0 = c /= Carro 
verificaCimaLado (a,b) (Mapa tamanho ((Relva,c:d):t)) 1 0 = c /= Arvore 
verificaCimaLado (a,b) (Mapa tamanho ((x,c:d):t)) 1 0 = True 
verificaCimaLado (a,b) (Mapa tamanho ((terr,c:d):t)) 1 anda = verificaCimaLado (a,b) (Mapa tamanho ((terr,d):t)) 1 (anda-1)
verificaCimaLado (a,b) (Mapa tamanho ((terr,c:d):t)) tam anda = verificaCimaLado (a,b) (Mapa tamanho (t)) (tam-1) anda 

verificaBaixo :: Coordenadas -> Mapa -> Int -> Int -> Bool 
verificaBaixo (a,b) (Mapa tamanho ((Estrada x2,c:d):t)) 0 0 = c /= Carro 
verificaBaixo (a,b) (Mapa tamanho ((Relva,c:d):t)) 0 0 = c /= Arvore 
verificaBaixo (a,b) (Mapa tamanho ((x,c:d):t)) 0 0 = True 
verificaBaixo (a,b) (Mapa tamanho ((terr,c:d):t)) 0 anda = verificaBaixo (a,b) (Mapa tamanho ((terr,d):t)) 0 (anda-1)
verificaBaixo (a,b) (Mapa tamanho ((terr,c:d):t)) tam anda = verificaBaixo (a,b) (Mapa tamanho (t)) (tam-1) anda 


animaJogo :: Jogo -> Jogada -> Jogo
que movimenta os obst ́aculos (de acordo com a velocidade) do terreno em
que se encontram), e o personagem, de acordo com a jogada dada: as jogadas
poss ́ıveis s ̃ao dadas pelo seguinte tipo de dados:
data Direc ̧c~ao = Cima
| Baixo
| Esquerda
| Direita
data Jogada = Parado
| Move Direc ̧c~ao
Note:
1. Numa estrada ou rio com velocidade v, os obst ́aculos devem mover-se
|v| unidades na direc ̧c ̃ao determinada.




animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador(a,b)) m@(Mapa tam x)) jogada = Jogo (moveJogador (Jogador (a,b)) jogada m) ((Mapa tem (andaTudo x)))

animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (a,b)) (Mapa l (terr, (h:t)))) jogada = 
-}


--animaJogo :: Jogo -> Jogada -> Jogo
--animaJogo 

moveJogador :: Direcao -> Jogador-> Jogador 
moveJogador Cima (Jogador (x,y)) = (Jogador (x,y-1))
moveJogador Baixo (Jogador (x,y)) = (Jogador (x,y+1))
moveJogador Esquerda (Jogador (x,y)) = (Jogador (x-1,y))
moveJogador Direita (Jogador (x,y)) = (Jogador (x+1,y))


validoMovimento :: Mapa -> Jogador -> Direcao -> Bool 
validoMovimento (Mapa l (h:t)) (Jogador (x,y)) Esquerda 
 | x == 0 = False 
 | otherwise = True 
validoMovimento (Mapa l (h:t)) (Jogador (x,y)) Direita
 | x == l = False 
 | otherwise = True
validoMovimento (Mapa l (h:t)) (Jogador (x,y)) Cima 
 | y == 0 = False 
 | otherwise = True 
validoMovimento (Mapa l (h:t)) (Jogador (x,y)) Baixo
 | y == length (h:t) = False
 | otherwise = True 

validoParado :: Mapa -> Jogador -> Jogada -> Bool
validoParado (Mapa l (h:t)) (Jogador (x,y)) (Parado) = True 



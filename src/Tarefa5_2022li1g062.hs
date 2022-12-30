{- |
Module      : Tarefa5_2022li1g062
Description : Certificar a criação de uma nova linha de mapa e a eliminação de outra
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/2023.
-}

module Tarefa5_2022li1g062 where

import LI12223
import Tarefa2_2022li1g062
import Test.HUnit 

{-
O objectivo desta tarefa  ́e implementar a fun ̧c ̃ao:
deslizaJogo :: Int -> Jogo -> Jogo
que, intuitivamente, faz com que a  ́ultima linha do mapa desapare ̧ca, ao
mesmo tempo que uma nova linha no topo do mapa seja criada. Mais
concretamente:
1. A  ́ultima linha do mapa deve ser removida, enquanto que, por outro
lado, uma nova linha deve ser adicionada ao topo do mapa. Em par-
ticular, o mapa resultante mant ́em o tamanho (i.e. comprimento) do
mapa original.
2. A coordenada y do jogador deve ser aumentada (em 1 unidade), re-
flectindo assim o efeito de que o jogador “ficou para tr ́as”.
O parˆametro Int corresponde ao inteiro aleat ́orio a ser passado `a fun ̧c ̃ao
estendeMapa previamente definida -}



{- | A função 'deslizaJogo' ira gerar uma nova linha de terreno e os respetivos obstaculos no topo do mapa fazendo com que a ultima desapareça.
A aleatoriedade da geraçao da nova linha tera em conta a soma entre o primeiro elemento e segundo elemento do par de coordenadas do jogador pelo que as linhas geradas no deslizamento do mapa serao sempre aleatorias dependendo da posiçao que o jogador que se encontrar.


== Exemplos de utilização:

@
>>> deslizaJogo ( Jogo (Jogador (5,0) ) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]) ) 
Jogo (Jogador (5,0)) (Mapa 5 [(Estrada 2,[Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])])
@

@
>>> deslizaJogo ( Jogo (Jogador (4,0) ) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]) ) 
Jogo (Jogador (4,0)) (Mapa 5 [(Rio 2,[Nenhum,Tronco,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])])
@

@
>>> deslizaJogo ( Jogo (Jogador (4,3) ) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]) ) 
Jogo (Jogador (4,3)) (Mapa 5 [(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])])
@

@
>>> deslizaJogo ( Jogo (Jogador (5,2) ) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]) )
Jogo (Jogador (4,3)) (Mapa 5 [(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])])
@

| -}

deslizaJogo :: Jogo -> Jogo
deslizaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) = (Jogo (Jogador (x,y)) mapa_novo)
    where mapa_novo = estendeMapa  (Mapa l (init (h:t))) seed  
          seed =  (((y*x)^3) +y) `div` (5+x)

{- |
Module      : Tarefa5_2022li1g062
Description : Certificar a criação de uma nova linha de mapa e a eliminação de outra
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/2023.
-}

module Tarefa5_2022li1g062 where

import LI12223
import Tarefa1_2022li1g062
import Tarefa2_2022li1g062
import Test.HUnit 


{- | 
A função 'deslizaJogo' ira gerar uma nova linha de terreno e os respetivos obstaculos no topo do mapa fazendo com que a ultima desapareça.
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

-}

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) (Mapa l (h:t))) = (Jogo (Jogador (x,y-1)) mapa_testado)
    where mapa_testado = if (mapaValido mapa_novo == False) then estendeMapa (Mapa l (init (h:t))) (seed+1) else mapa_novo
          mapa_novo = estendeMapa  (Mapa l (init (h:t))) seed  
          


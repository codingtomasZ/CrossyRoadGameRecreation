{- |
Module      : Tarefa3_2022li1g062
Description : Movimentação do personagem e obstáculos
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}

module Tarefa3_2022li1g062 where

import LI12223

animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) (Mapa l t)) jog 
 | jog == Move Cima = validoMovimento3 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Cima)
 | jog == Move Baixo = validoMovimento4 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Baixo)
 | jog == Move Esquerda = validoMovimento1 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Esquerda)
 | jog == Move Direita = validoMovimento2 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Direita)
 | jog == Parado = validoParado (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Parado)
animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) ((Mapa l linhas))) j = (Jogo (moveJogador j (Jogador (x,y)) ) ((Mapa l linhas)))


{- | A função "moveJogador" pretende animar o jogador, mediante a jogada escolhida ser fazer o jogador andar para cima, para baixo, para esquerda ou para a direita.
Clicando em /Direcao/ e /Jogador/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> moveJogador Move Cima ( Jogador (0,0) )
Jogador (0,1)
@

@
>>> moveJogador Move Esquerda ( Jogador (2,1) )
Jogador (1,1)
@

| -}


moveJogador :: Jogada -> Jogador-> Jogador 
moveJogador (Move Cima) (Jogador (x,y)) = (Jogador (x,y-1))
moveJogador (Move Baixo) (Jogador (x,y)) = (Jogador (x,y+1))
moveJogador (Move Esquerda) (Jogador (x,y)) = (Jogador (x-1,y))
moveJogador (Move Direita) (Jogador (x,y)) = (Jogador (x+1,y))
moveJogador Parado (Jogador (x,y)) = (Jogador (x,y)) 


{- | A função "validoMovimento" pretende limitar os coordenadas possíveis do jogador, ao limite do mapa, não permitindo que ele saia para fora deste.
Clicando em /Jogo/ e /Jogada/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> validoMovimento  (Mapa 3 (Rio 1, (Tronco, Nenhum, Nenhum))) (Jogador (0,0)) Move Cima
False
@

@
>>> validoMovimento  (Mapa 3 (Rio 1, (Tronco, Nenhum, Nenhum))) (Jogador (0,0)) Move Esquerda
False
@

| -}


validoMovimento1 :: Jogo -> Jogada -> Jogo 
validoMovimento1 (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Esquerda) 
   | (x == 0 && (y <= length (h:t))) = (Jogo (Jogador (x,y)) (Mapa l (h:t)))
   | otherwise = (Jogo (Jogador (x-1,y)) (Mapa l (h:t)))
validoMovimento2 :: Jogo -> Jogada -> Jogo 
validoMovimento2 (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Direita)
   | (x == (l -1) && y <= length (h:t)) =  (Jogo (Jogador (x,y)) (Mapa l (h:t)))
   | otherwise = (Jogo (Jogador (x+1,y)) (Mapa l (h:t)))
validoMovimento3 :: Jogo -> Jogada -> Jogo 
validoMovimento3 (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Cima)
  | ((y == 0) && (x <= l - 1) && (x>=0) ) = (Jogo (Jogador (x,y)) (Mapa l (h:t)))
  | otherwise = (Jogo (Jogador (x,y-1)) (Mapa l (h:t)))
validoMovimento4 :: Jogo -> Jogada -> Jogo 
validoMovimento4 (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Baixo)
  | (y == (length (h:t) -1 )  && (x <= l-1) ) = (Jogo (Jogador (x,y)) (Mapa l (h:t)))
  | otherwise= (Jogo (Jogador (x,y+1)) (Mapa l (h:t)))



-- Parado 

validoParado :: Jogo -> Jogada -> Jogo 
validoParado (Jogo (Jogador (x,y)) (Mapa l t)) (Parado) = (Jogo (Jogador (x,y)) (Mapa l (moveObs t))) 



moveObs :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moveObs [] = []
moveObs ((Relva, obs):res) = (Relva,obs) : moveObs res 
moveObs ((Rio v , (h:t)):res ) | v > 0 = moveObs ((Rio (v-1), last t : init (h:t)) : res )
                               | v < 0 = moveObs ((Rio (v+1), t ++ [h]): res)
                               | v == 0 = (Rio 0 , (h:t)) : moveObs res 
moveObs ((Estrada v , (h:t)): res ) | v == 0 = moveObs (( Estrada (v-1), last t : init (h:t)):res)
                                           | v > 0 = moveObs ((Estrada (v-1), last t : init (h:t)) : res) 
                                           | v < 0 = moveObs ((Estrada (v+1), t ++ [h]): res)










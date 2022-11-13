{- |
Module      : Tarefa3_2022li1g062
Description : Movimentação do personagem e obstáculos
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g062 where

import LI12223

--DA ERRO mas tentei 
animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) ((Mapa l t)):res) jog = moveJogador (Jogo (Jogador (x,y)) ((Mapa l t)):res)

-- Exercicio 2 

moveJogador :: Direcao -> Jogador-> Jogador 
moveJogador Cima (Jogador (x,y)) = (Jogador (x,y-1))
moveJogador Baixo (Jogador (x,y)) = (Jogador (x,y+1))
moveJogador Esquerda (Jogador (x,y)) = (Jogador (x-1,y))
moveJogador Direita (Jogador (x,y)) = (Jogador (x+1,y))

-- Exercicio 4 

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

-- Parado 

validoParado :: Mapa -> Jogador -> Jogada -> Bool
validoParado (Mapa l (h:t)) (Jogador (x,y)) (Parado) = True 

-- Exercicio 3 

procuraTronco :: [Obstaculo] -> Bool 
procuraTronco [] = False 
procuraTronco (h1:h2)
   | h1 == Tronco = True 
   | otherwise = procuraTronco h2 


moveObs :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moveObs [] = []
moveObs ((Relva, obs):res) = (Relva,obs) : moveObs res 
moveObs ((Rio v , (h:t)):res ) | v > 0 = moveObs ((Rio (v-1), last t : init (h:t)) : res )
                                      | v < 0 = moveObs ((Rio (v+1), t ++ [h]): res)
                                      | v == 0 = (Rio 0 , (h:t)) : moveObs res 
moveObs ((Estrada v , (h:t)): res ) | v == 0 = moveObs (( Estrada (v-1), last t : init (h:t)):res)
                                           | v > 0 = moveObs ((Estrada (v-1), last t : init (h:t)) : res )
                                           | v < 0 = moveObs ((Estrada (v+1), t ++ [h]): res)








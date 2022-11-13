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
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo = animaJogo1 && animaJogo2 && animaJogo3 -}

{- | A função "animaJogo1" pretende criar a movimentação dos terrenos animados, mediante a velocidade dada aos obstáculos móveis, os carros e troncos
Clicando em /Jogo/ e /Jogada/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> animaJogo1 Jogo ( Jogador (0,0) ) (Mapa 5 [(Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) )
(Jogo (Jogador (0,0) )(Mapa 5 [(Estrada 1, [Carro, Carro, Nenhum, Carro, Nenhum])]))
@

@
>>> animaJogo1 Jogo ( Jogador (0,0) ) (Mapa 5 [(Rio -2, [Tronco, Nenhum, Tronco, Nenhum, Tronco])]) )
(Jogo (Jogador (0,0) )(Mapa 5 [(Rio -2, [Tronco, Nenhum, Tronco, Tronco, Nenhum])]))
@

| -}

animaJogo1 :: Jogo -> Jogada -> Jogo
animaJogo1 (Jogo j1 (Mapa l [])) j2 = []
animaJogo1 (Jogo j1 (Mapa l (Rio v, o):t)) j2 = ( Jogo j1 (Mapa l (aux3_1( (Rio v o):(aux3_1 t))) ) )  

aux3_1 :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
aux3_1 [] = []
aux3_1 ((Rio v, o):t)
   | v > 0 = aux3_1 (Rio v, ((drop v o) ++ (take v o)))++(aux3_1 t)
   | v < 0 = aux3_1 (Rio v, ((drop (-v) o) ++ (take (-v) o)))++(aux3_1 t)

{- | A função "animaJogo2" pretende animar o jogador, mediante a jogada escolhida ser fazer o jogador andar para cima, para baixo, para esquerda ou para a direita.
Clicando em /Direcao/ e /Jogador/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> animaJogo2 Move Cima ( Jogador (0,0) )
Jogador (0,1)
@

@
>>> animaJogo2 Move Esquerda ( Jogador (2,1) )
Jogador (1,1)
@

| -}

animaJogo2 :: Direcao -> Jogador-> Jogador 
animaJogo2 Cima (Jogador (x,y)) = (Jogador (x,y-1))
animaJogo2 Baixo (Jogador (x,y)) = (Jogador (x,y+1))
animaJogo2 Esquerda (Jogador (x,y)) = (Jogador (x-1,y))
animaJogo2 Direita (Jogador (x,y)) = (Jogador (x+1,y))

{- | A função "animaJogo3" pretende animar o jogador para que, quando este estiver parado em cima de um tronco, ele se movimente de modo a manter-se sempre em cima do tronco.
Clicando em /Jogo/ e /Jogada/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> animaJogo3 Jogo (Jogador (0,0)) (Mapa 3 (Rio 1, (Tronco, Nenhum, Nenhum))) Parado
Jogo (Jogador (1,0) ) (Mapa 3 (Rio 1, (Nenhum, Tronco, Nenhum) ))
@

@
>>> animaJogo3 Jogo (Jogador (2,0)) (Mapa 3 (Rio (-1), (Tronco, Nenhum, Tronco))) Parado
Jogo (Jogador (1,0) ) (Mapa 3 (Rio 1, (Nenhum, Tronco, Tronco) ))
@

| -}

animaJogo3 Jogo -> Jogada -> Jogo
animaJogo3 (Jogo (x,y) (Mapa l []))
animaJogo3 (Jogo (x,y) (Mapa l (Rio v, (h:t)):t)) Parado = (Jogo ((aux3_3 0 (h:t)),y) (Mapa l (Rio v, (h:t)):t))

aux3_3 :: Int -> [Obstaculo] -> Bool
aux3_3 x [] = False
aux3_3 x (h:t)
  | h == Tronco = x
  | otherwise = procuraTronco (x+1) h2

{- | A função "animaJogo4" pretende limitar os coordenadas possíveis do jogador, ao limite do mapa, não permitindo que ele saia para fora deste.
Clicando em /Jogo/ e /Jogada/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> animaJogo4  (Mapa 3 (Rio 1, (Tronco, Nenhum, Nenhum))) (Jogador (0,0)) Move Cima
False
@

@
>>> animaJogo4  (Mapa 3 (Rio 1, (Tronco, Nenhum, Nenhum))) (Jogador (0,0)) Move Esquerda
False
@

| -}

animaJogo4 :: Mapa -> Jogador -> Direcao -> Bool 
animaJogo4 (Mapa l (h:t)) (Jogador (x,y)) Esquerda 
 | x == 0 = False 
 | otherwise = True 
animaJogo4 (Mapa l (h:t)) (Jogador (x,y)) Direita
 | x == l = False 
 | otherwise = True
animaJogo4 (Mapa l (h:t)) (Jogador (x,y)) Cima 
 | y == 0 = False 
 | otherwise = True 
animaJogo4 (Mapa l (h:t)) (Jogador (x,y)) Baixo
 | y == length (h:t) = False
 | otherwise = True 









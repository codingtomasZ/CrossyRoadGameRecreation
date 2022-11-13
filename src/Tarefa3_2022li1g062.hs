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

animaJogo2 :: Jogo -> Jogada -> Jogo
animaJogo2 (Jogo (Jogador (x,y)) m) Parado = Jogo (Jogador (x,y)) m 
animaJogo2 (Jogo (Jogador (x,y)) m) (Move d) 
   |d == Cima == (Jogo (Jogador (x,y+1)) m) 
   |d == Baixo == (Jogo (Jogador (x,y-1)) m)


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



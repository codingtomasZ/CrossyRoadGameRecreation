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
animaJogo2 :: Direcao -> Jogador-> Jogador 
animaJogo2 Cima (Jogador (x,y)) = (Jogador (x,y-1))
animaJogo2 Baixo (Jogador (x,y)) = (Jogador (x,y+1))
animaJogo2 Esquerda (Jogador (x,y)) = (Jogador (x-1,y))
animaJogo2 Direita (Jogador (x,y)) = (Jogador (x+1,y))

--Exercício 3

animaJogo3 Jogo -> Jogada -> Jogo
animaJogo3 (Jogo (x,y) (Mapa l []))
animaJogo3 (Jogo (x,y) (Mapa l (Rio v, (h:t)):t)) Parado = (Jogo ((aux3_3 0 (h:t)),y) (Mapa l (Rio v, (h:t)):t))

aux3_3 :: Int -> [Obstaculo] -> Bool
aux3_3 x [] = False
aux3_3 x (h:t)
  | h == Tronco = x
  | otherwise = procuraTronco (x+1) h2

--Exercício 4 

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


--Exercício 6
{-
animaJogo6 :: Jogo -> Jogada -> Jogo
animaJogo6 (Jogo j1 (Mapa l (Relva v, o):t)) 
-}







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
animaJogo (Jogo (Jogador (x,y)) (Mapa l t)) jog 
 | jog == Move Cima = validoMovimento3 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Cima)
 | jog == Move Baixo = validoMovimento4 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Baixo)
 | jog == Move Esquerda = validoMovimento1 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Esquerda)
 | jog == Move Direita = validoMovimento2 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Direita)
 | jog == Parado = validoParado (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Parado)
animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) ((Mapa l linhas))) j = (Jogo (moveJogador j (Jogador (x,y)) ) ((Mapa l linhas))) -}



animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) (Mapa l t)) jog 
 | jog == Move Cima =  ( atrop ( movitron   (validoMovimento3 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Cima)) ) ) 
 | jog == Move Baixo =   ( atrop (movitron ( validoMovimento4 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Baixo) ) ) ) 
 | jog == Move Esquerda = ( atrop  (movitron (validoMovimento1 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Esquerda) ) ) ) 
 | jog == Move Direita = (atrop (movitron (validoMovimento2 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Direita) ) ) )
 | jog == Parado =    atrop ( validoParado (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Parado) ) 

validoParado :: Jogo -> Jogada -> Jogo 
validoParado (Jogo (Jogador (x,y)) (Mapa l t)) (Parado) = (Jogo (Jogador (x,y)) (Mapa l (moveObs t)))  



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

movitron :: Jogo -> Jogada -> Jogo 
movitron (Jogo (Jogador (x , y) ) (Mapa l ((Rio v ,o ):t) ) ) jog 
 | ((!!) o x == Tronco) && (jog == Move Cima) = (Jogo (Jogador (x , y-1) ) (Mapa l ((Rio v ,o ):t) ) ) 
 | ((!!) o x == Tronco) && (jog == Move Baixo) = (Jogo (Jogador (x , y+1) ) (Mapa l ((Rio v ,o ):t) ) ) 
 | ((!!) o x == Tronco) && (jog == Move Esquerda) && v < 0 = (Jogo (Jogador (x -1 + v  , y) ) (Mapa l ((Rio v ,o ):t) ) ) 
 | ( (!!) o x == Tronco) && (jog == Move Direita) && v> 0 = (Jogo (Jogador (x+1+ v , y) ) (Mapa l ((Rio v ,o ):t) ) ) 
 | otherwise = (Jogo (Jogador (x , y) ) (Mapa l ((Rio v ,o ):t) ) )


postronco :: [Obstaculo] -> Obstaculo -> Int
postronco (x:xs) n = atmaux1 (x:xs) n 0

atmaux1 :: [Obstaculo] -> Obstaculo -> Int -> Int 
atmaux1 [] n y = y 
atmaux1 (x:xs) n y = if x == Tronco 
                    then y 
                    else atmaux1 xs n (y+1) 



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






moveObs :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moveObs [] = []
moveObs ((Relva, obs):res) = (Relva,obs) : moveObs res 
moveObs ((Rio v , (h:t)):res ) | v > 0 = moveObs ((Rio (v-1), last t : init (h:t)) : res )
                               | v < 0 = moveObs ((Rio (v+1), t ++ [h]): res)
                               | v == 0 = (Rio 0 , (h:t)) : moveObs res 
moveObs ((Estrada v , (h:t)): res ) | v == 0 = moveObs (( Estrada (v-1), last t : init (h:t)):res)
                                           | v > 0 = moveObs ((Estrada (v-1), last t : init (h:t)) : res) 
                                           | v < 0 = moveObs ((Estrada (v+1), t ++ [h]): res)

{-
atrop :: Jogo -> Jogada -> Jogo 
atrop (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,o):t))) jog 
 | ( v >= ( x - poscarro o Carro ) ) && (x - poscarro o Carro > 0 ) && (jog == (Parado) ) = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 | ( v <= ( x - poscarro o Carro ) ) && (x - poscarro o Carro < 0 ) && (jog == (Parado) ) = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 | ((x - poscarro o Carro ) == 1) && (v > 0 ) && (  jog == Parado) = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 ! ( (x - poscarro o Carro > 0) &&  v > 0 &&    ( Move Esquerda)   ) 
 |  v == x && x < v  && jog = (Move Esquerda) = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 | otherwise = (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,o):t))) -}

atrop :: Jogo -> Jogada -> Jogo 
atrop (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,o):t))) jog 
 | ( v >= ( x - poscarro o Carro ) ) && (x - poscarro o Carro > 0 ) && jog == Parado = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 | ( v <= ( x - poscarro o Carro ) ) && (x - poscarro o Carro < 0 ) && jog == Parado = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 | ( v <= ( x - poscarro o Carro ) ) && (x - poscarro o Carro < 0 ) && jog == Move Direita = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 | ( v >= ( x - poscarro o Carro ) ) && (x - poscarro o Carro > 0 ) && jog == Move Esquerda = (Jogo (Jogador (0,0)) (Mapa l ((Estrada v,o):t)))
 | otherwise = (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,o):t))) 
 


poscarro :: [Obstaculo] -> Obstaculo -> Int
poscarro (x:xs) n = atmaux (x:xs) n 0


atmaux :: [Obstaculo] -> Obstaculo -> Int -> Int 
atmaux [] n y = y 
atmaux (x:xs) n y = if x == Carro
                    then y 
                    else atmaux xs n (y+1)



posarvore :: [Obstaculo] -> Obstaculo -> Int
posarvore (x:xs) n = atmaux2 (x:xs) n 0 

atmaux2 :: [Obstaculo] -> Obstaculo -> Int -> Int 
atmaux2 [] n y = y 
atmaux2 (x:xs) n y = if x == Arvore  
                    then y 
                    else atmaux2 xs n (y+1)


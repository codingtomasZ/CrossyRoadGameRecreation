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
animaJogo (Jogo (Jogador (x,y)) ((Mapa l linhas))) j = (Jogo (moveJogador j (Jogador (x,y)) ) ((Mapa l linhas))) 



animaJogo :: Jogo -> Jogada -> Jogo 
animaJogo (Jogo (Jogador (x,y)) (Mapa l t)) jog 
 | jog == Move Cima =  ( atrop ( movitron   (validoMovimento3 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Cima)) ) ) 
 | jog == Move Baixo =   ( atrop (movitron ( validoMovimento4 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Baixo) ) ) ) 
 | jog == Move Esquerda = ( atrop  (movitron (validoMovimento1 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Esquerda) ) ) ) 
 | jog == Move Direita = (atrop (movitron (validoMovimento2 (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Move Direita) ) ) )
 | jog == Parado =    atrop ( validoParado (Jogo (Jogador (x,y)) ((Mapa l (moveObs t)))) (Parado) ) 

validoParado :: Jogo -> Jogada -> Jogo 
validoParado (Jogo (Jogador (x,y)) (Mapa l t)) (Parado) = (Jogo (Jogador (x,y)) (Mapa l (moveObs t)))

-}

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
 | ((!!) o x == Tronco) && (jog == Move Direita) && v> 0 = (Jogo (Jogador (x+1+ v , y) ) (Mapa l ((Rio v ,o ):t) ) ) 
 | otherwise = (Jogo (Jogador (x , y) ) (Mapa l ((Rio v ,o ):t) ) )


posicao_tronco :: [Obstaculo] -> [Int]
posicao_tronco (h:t) = lista_posicoes (h:t) Tronco 0 



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

validoMovimento :: Jogo ->Jogada -> Jogo
validoMovimento jogo jogada
  | jogada == Move Esquerda = validoMovimento1 jogo
  | jogada == Direita = validoMovimento2 jogo jogado
-- juntar todos os casos numa só função

validoMovimento1 :: Jogo -> Jogo 
validoMovimento1 (Jogo (Jogador (x,y)) (Mapa l (h:t)))
   | (x == 0 && (y <= length (h:t))) = (Jogo (Jogador (x,y)) (Mapa l (h:t)))
   | otherwise = (Jogo (Jogador (x-1,y)) (Mapa l (h:t)))
-- caso a linha onde ele está seja relva, ver se tem uma árvore à esquerda
 
-- se x-1 pertencer à lista, tem uma árvore à esquerda
-- se (elem (x-1) posarvore) quer dixer que tem uma árvore à esquerda e que mantem a posição 

validoMovimento2 :: Jogo -> Jogada -> Jogo 
validoMovimento2 (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Direita)
   | (x == (l -1) && y <= length (h:t)) =  (Jogo (Jogador (x,y)) (Mapa l (h:t)))
   | otherwise = (Jogo (Jogador (x+1,y)) (Mapa l (h:t)))
-- caso a linha onde ele está seja relva, ver se tem uma árvore à direita


validoMovimento3 :: Jogo -> Jogada -> Jogo 
validoMovimento3 (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Cima)
  | ((y == 0) && (x <= l - 1) && (x>=0) ) = (Jogo (Jogador (x,y)) (Mapa l (h:t)))
  | otherwise = (Jogo (Jogador (x,y-1)) (Mapa l (h:t)))
-- caso a linha a seguir à linha onde o jogador está seja Relva, ver se tem uma árvore à frente (xjogador = xarvore)


validoMovimento4 :: Jogo -> Jogada -> Jogo 
validoMovimento4 (Jogo (Jogador (x,y)) (Mapa l (h:t))) (Move Baixo)
  | (y == (length (h:t) -1 )  && (x <= l-1) ) = (Jogo (Jogador (x,y)) (Mapa l (h:t)))
  | otherwise= (Jogo (Jogador (x,y+1)) (Mapa l (h:t)))
-- caso a linha antes da linha onde o jogador está seja Relva, ver se tem uma árvore atras (xjogador = xarvore)


    -- M O V E   O B S T A C U L O S -- 


moveObs :: Int -> [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moveObs _ [] =  [] 
moveObs l ((Relva, obstaculos):t)=(Relva, obstaculos):(moveObs l t)
moveObs l ((Rio v, obstaculos):t)
  | v > 0 = (Rio v, ((drop (l-v) obstaculos) ++ (take (l-v) obstaculos))):(moveObs l t)
  | v < 0 = (Rio v, ((drop (-v) obstaculos) ++ (take (-v) obstaculos))):(moveObs l t)
  | v == 0 = (Rio v, obstaculos):(moveObs l t)
moveObs l ((Estrada v, obstaculos):t)
  | v > 0 = (Estrada v, ((drop (l-v) obstaculos) ++ (take (l-v) obstaculos))):(moveObs l t)
  | v < 0 = (Estrada v, ((drop (-v) obstaculos) ++ (take (-v) obstaculos))):(moveObs l t)
  | v == 0 = (Estrada v, obstaculos):(moveObs l t)


-- FUNÇÃO ATROPELAMENTO --

atropelamento :: Jogo -> Jogada -> Mapa 
atropelamento (Jogo (Jogador (x,y)) (Mapa l (h:t))) jogada = if linha_jogador (h:t) y == (Estrada v, obstaculos)
                                                           then (Mapa l (mapa_actualizado))
                                                           else (Mapa l (h:t))
    where linha_actual = linha_jogador (h:t) y 
          linha_atualizada = atropelamento_aux linha_actual l x jogada
          mapa_actualizado = (take (y-1) (h:t)) ++ [linha_atualizada] ++ (drop y (h:t))
          v = velocidade_da_linha linha_actual
          obstaculos = obstaculos_da_linha linha_actual


atropelamento_aux :: (Terreno, [Obstaculo]) -> Int -> Int -> Jogada -> (Terreno, [Obstaculo])
atropelamento_aux (Estrada v, (h:t)) l x jogada 
  | v > 0 = atropelamento_aux_e (Estrada v, (h:t)) l x jogada
  | v < 0 = atropelamento_aux_d (Estrada v, (h:t)) l x jogada
  |otherwise = (Estrada v, (h:t))


atropelamento_aux_e :: (Terreno, [Obstaculo]) -> Int -> Int -> Jogada -> (Terreno, [Obstaculo])
atropelamento_aux_e (Estrada v, (h:t)) l x jogada
  | ( v >= distancia_e ) = (Estrada v, (drop ( l - distancia_e ) (h:t)) ++ (take (l - distancia_e) (h:t)) )
  | ( distancia_e == 1 ) && jogada == Move Esquerda = (Estrada v, (h:t))
  | ( v <= distancia_e ) && jogada == Parado = (Estrada v, (h:t))
  | otherwise = (Estrada v, (h:t))
         where lista_pos = posicao_carro (h:t)
               poscarro = posicao_carro_prox_e lista_pos x 
               distancia_e = (x- poscarro)


atropelamento_aux_d :: (Terreno, [Obstaculo]) -> Int -> Int -> Jogada -> (Terreno, [Obstaculo])
atropelamento_aux_d (Estrada v, (h:t)) l x jogada
  | ( distancia_d == 1 ) && jogada == Move Direita = (Estrada v, (h:t))
  | ( (-v) <= distancia_d ) && jogada == Parado = (Estrada v, (h:t))
  | ( (-v) >= distancia_d ) = (Estrada v, (drop distancia_d (h:t)) ++ (take distancia_d (h:t)))
  | otherwise = (Estrada v, (h:t))
         where lista_pos = posicao_carro (h:t) 
               poscarro = posicao_carro_prox_d lista_pos x
               distancia_d = -(x- poscarro)


linha_jogador :: [(Terreno, [Obstaculo])] -> Int -> (Terreno, [Obstaculo])
linha_jogador linhas y = (!!) (reverse linhas) y



    -- E S T R A D A   E   O B S T A C U L O S --

posicao_carro_prox_e :: [Int] -> Int -> Int
posicao_carro_prox_e [] _ = 0
posicao_carro_prox_e (h:t) x 
  | last (h:t) > x = posicao_carro_prox_e (init (h:t)) x
  | otherwise = last (h:t)  


posicao_carro_prox_d :: [Int] -> Int -> Int
posicao_carro_prox_d [] _ = 0
posicao_carro_prox_d (h:t) x 
  | h < x = posicao_carro_prox_d t x
  | otherwise = h


posicao_carro :: [Obstaculo] -> [Int]
posicao_carro (h:t) = lista_posicoes (h:t) Carro 0


    -- R E L V A   E   O B S T A C U L O S -- 

posicao_arvore :: [Obstaculo] -> [Int]
posicao_arvore (h:t) = lista_posicoes (h:t) Arvore 0

    -- G E R A L   C O N T A D O R   D E   O B S T A C U L O S --


lista_posicoes :: [Obstaculo] -> Obstaculo -> Int -> [Int] 
lista_posicoes [] n y = [] 
lista_posicoes (h:t) n y 
  | h == Carro = y:(lista_posicoes t n (y+1)) 
  | otherwise = lista_posicoes t n (y+1)


velocidade_da_linha :: (Terreno, [Obstaculo]) -> Velocidade
velocidade_da_linha (Estrada v, o) = v
velocidade_da_linha (Rio v, o) = v


obstaculos_da_linha :: (Terreno, [Obstaculo]) -> [Obstaculo]
obstaculos_da_linha (Estrada v, o) = o
obstaculos_da_linha (Rio v, o) = o

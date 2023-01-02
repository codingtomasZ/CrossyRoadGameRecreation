{- |
Module      : Tarefa4_2022li1g062
Description : Determinar se o jogo terminou
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/2023.
-}

module Tarefa4_2022li1g062 where

import LI12223
import Tarefa3_2022li1g062


{-|
Funçao 'jogoTerminou'. Caso a funçao que indica que o Jogador se encontra fora do mapa ('jogoTerminou1') valide para True ou caso a funçao que verifica se o jogador se encontra na agua ('jogoTerminou2') valide True ou caso a funçao que indica que o jogador se encontra na mesma posiçao de Carro ('jogoTerminou3') valide para True, entao 'jogoTerminou' retornara True, caso contrario retornara False.

== Exemplos de utilizaçao: 1º caso -> Jogador sobrevive. 2º caso -> Jogador fora do mapa. 3º caso -> Coordenadas do jogador correspondem a posicao de Carro e o Jogador e eliminado.

@
>>> jogoTerminou (Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco])])  )
False
@

@
>>>  jogoTerminou (Jogo (Jogador (2,5)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco])])  
True
@

@
>>>  jogoTerminou (Jogo (Jogador (0,3)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco])])  )
True
@

-}

jogoTerminou :: Jogo -> Bool
jogoTerminou jogo = if (jogoTerminou1 jogo) == True || (jogoTerminou2 jogo ) == True || (jogoTerminou3 jogo ) == True 
                   then True 
                   else False 


{-|
A função "jogoTerminou1" avalia se o jogador perdeu o jogo devido a 4 casos diferentes. No primeiro é testado se o jogador perdeu porque a sua coordenada x for negativa, ou seja, estiver fora do mapa, pelo lado esquerdo. No segundo caso é testado se a posição do jogador tem a coordenada y negativa, ou seja, o jogador está a sair do mapa pela parte de baixo. O terceiro caso testa se o valor da coordenada x é superior à largura do mapa, ou seja, se o jogador está fora do mapa pelo lado direito. O quarto e último caso testa se o valor da coordenada y é superior ao comprimento (de baixo para cima) do mapa, ou seja, se o jogador sai do mapa pela parte de cima. 
Clicando em /Mapa/ e /Jogador/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> jogoTerminou1 (Jogo (Jogador (1,2)) (Mapa 5 [(Rio (-2), [Nenhum, Tronco, Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])]) ) 
True 
@

@
>>> jogoTerminou1 (Jogo (Jogador (5,1)) (Mapa 5 [(Rio (-2), [Nenhum, Tronco, Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])]) ) 
True
@

-}



jogoTerminou1 :: Jogo -> Bool 
jogoTerminou1 (Jogo (Jogador (x,y)) (Mapa l (h:t)))  = if x < 0 || y < 0 || x > (l-1) || y > ((length (h:t))-1)
                                             then True
                                             else False 


{-| 
A função "jogoTerminou2" testa se o jogador, quando está numa linha em que o terreno é rio, está numa coordenada "Nenhum", ou seja, está afogado e então perde o jogo.

== Exemplos de utilizaçao: 

@
>>> jogoTerminou2 (Jogo (Jogador (1,1)) (Mapa 5 [(Rio (-2), [Nenhum, Tronco, Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])]) )
False
@

@
>>> jogoTerminou2 (Jogo (Jogador (0,1)) (Mapa 5 [(Rio (-2), [Nenhum, Tronco, Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])]) )
True
@

-}

jogoTerminou2 :: Jogo -> Bool
jogoTerminou2 (Jogo (Jogador (x,y)) (Mapa l linhas)) = if ((!!) (reverse linhas) y) == (Rio v, obstaculos) 
                                                      then jogoTerminou2_aux ((!!) (reverse linhas) y) x
                                                      else False
    where v = velocidade_da_linha ((!!) (reverse linhas) y)
          obstaculos = obstaculos_da_linha ((!!) (reverse linhas) y)

{-|

Funçao auxiliar para 'jogoTerminou2'. Verifica se o primeiro elemento do par de coordenadas do Jogador se encontra na mesma posicao de Nenhum. Se sim, o Jogador sera eliminado (True), caso contrario sobrevivera (False).

== Exemplos de utilizaçao:

@
>>>  jogoTerminou2_aux (Rio 2 ,  [Tronco, Tronco, Nenhum]) 1 
False
@

@
>>>  jogoTerminou2_aux (Rio 2 ,  [Tronco, Tronco, Nenhum]) 2
True
@
-}


jogoTerminou2_aux :: (Terreno, [Obstaculo]) -> Int -> Bool
jogoTerminou2_aux (Rio v, obstaculos) x 
  | (!!) obstaculos x == Nenhum = True
  | otherwise = False 



{- | A função "jogoTerminou3" testa se o jogador, quando está numa linha em que o terreno é Estrada, está numa coordenada "Carro", ou seja, está atropelado e então perde o jogo.


@
>>> jogoTerminou3 (Jogo (Jogador ( 0, 2) ) (Mapa 4 [(Estrada 2 , [Carro, Carro , Nenhum , Carro ]),(Rio 2, [Tronco , Tronco , Nenhum, Tronco ]),(Estrada (-1), [Carro , Carro , Nenhum , Nenhum ])]) ) 
True
@

@
>>> jogoTerminou3 (Jogo (Jogador ( 2, 2) ) (Mapa 4 [(Estrada 2 , [Carro, Carro , Nenhum , Carro ]),(Rio 2, [Tronco , Tronco , Nenhum, Tronco ]),(Estrada (-1), [Carro , Carro , Nenhum , Nenhum ])]) ) 
False
@

|-}

jogoTerminou3 :: Jogo -> Bool
jogoTerminou3 (Jogo (Jogador (x,y)) (Mapa l linhas)) = if ((!!) (reverse linhas) y) == (Estrada v, obstaculos) 
                                                      then jogoTerminou3_aux ((!!) (reverse linhas) y) x
                                                      else False
         where v = velocidade_da_linha ((!!) (reverse linhas) y)
               obstaculos = obstaculos_da_linha ((!!) (reverse linhas) y)

{-|

Funçao auxiliar para 'jogoTerminou3'. Verifica se o primeiro elemento do par de coordenadas do Jogador se encontra na mesma posicao do Carro. Se sim, o Jogador sera eliminado (True), caso contrario sobrevivera (False).

== Exemplos de utilizaçao:

@
>>>  jogoTerminou3_aux (Estrada 1, [Carro, Carro, Nenhum]) 2
False
@

@
>>>  jogoTerminou3_aux (Estrada (-2), [Carro, Carro]) 0
True
@
-}

jogoTerminou3_aux :: (Terreno, [Obstaculo]) -> Int -> Bool
jogoTerminou3_aux (Estrada v, obstaculos) x 
  | (!!) obstaculos x == Carro = True
  | otherwise = False

{- |
Module      : Tarefa4_2022li1g062
Description : Determinar se o jogo terminou
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/2023.
-}
module Tarefa4_2022li1g062 where

import LI12223

import Test.HUnit 

{- 
O objectivo desta tarefa  ́e implementar a fun ̧c ̃ao:
jogoTerminou :: Jogo -> Bool
que indica se o jogador perdeu o jogo, onde True significa que sim. Para isso
deve testar se o jogador se encontra fora do mapa, na  ́agua, ou “debaixo”
de um carro (i.e. na mesma posi ̧c ̃ao de um carro.)

Ao longo de um mapa o jogador ter ́a que atravessar:
1. Rios, onde n ̃ao poder ́a cair `a  ́agua e, para isso, ter ́a que saltar para
cima de um dos troncos.
2. Estradas, onde ter ́a que evitar ser atropelado por um carro.
3. Relva, onde ter ́a que contornar as  ́arvores.
Para evitar que o jogador simplesmente permane ̧ca na mesma posi ̧c ̃ao
o tempo todo, o mapa ir ́a automaticamente deslizar ao fim de um algum
tempo. Caso o jogador fique para tr ́as, isto  ́e, deixe de estar vis ́ıvel no
mapa, perde.
O jogador pode mover-se nas quatro direc ̧c ̃oes poss ́ıveis sem, contudo,
sair do mapa. Com efeito, as  ́unicas situa ̧c ̃oes em que o jogador pode escapar
do mapa s ̃ao:
1. Quando o mapa desliza e o jogador fica para trás.
2. Quando o tronco em que o jogador se encontra eventualmente desapa-
rece do mapa.
Em ambas as situa ̧c ̃oes, o jogador perde e o jogo termina imediatemente.
Num mesmo rio ou estrada, troncos e carros deslocam-se numa direc ̧c ̃ao
comum e a uma velocidade constante. Eventualmente estes obst ́aculos sair ̃ao
do mapa por um dos lados, voltando a reaparecer no lado oposto, como se
os limites do mapa fossem wormholes.
-}



jogoTerminou :: Mapa -> Jogador -> Bool
jogoTerminou = if jogoTerminou1 == True || jogoTerminou2 == True || jogoTerminou3 == True 
               then True 
               else False 

{- | A função "jogoTerminou1" avalia se o jogador perdeu o jogo devido a 4 casos diferentes. No primeiro é testado se o jogador perdeu porque a sua coordenada x for negativa, ou seja, estiver fora do mapa, pelo lado esquerdo. No segundo caso é testado se a posição do jogador tem a coordenada y negativa, ou seja, o jogador está a sair do mapa pela parte de baixo. O terceiro caso testa se o valor da coordenada x é superior à largura do mapa, ou seja, se o jogador está fora do mapa pelo lado direito. O quarto e último caso testa se o valor da coordenada y é superior ao comprimento (de baixo para cima) do mapa, ou seja, se o jogador sai do mapa pela parte de cima. 
Clicando em /Mapa/ e /Jogador/ é possivel obter mais informações relativamente a estas funções.

== Exemplos de utilização:

@
>>> jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (2,-2) ) )
True 
@

@
>>> jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (2,2) ) )
False
@

| -}

jogoTerminou1 :: Mapa -> Jogador -> Bool 
jogoTerminou1 (Mapa l [(t, o)]) (Jogador (x,y)) = if x < 0 || y < 0 || x > l || y > length ([(t,o)]) 
                                                then True
                                                else False


{- | A função "jogoTerminou2" testa se o jogador, quando está numa linha em que o terreno é rio, está numa coordenada "Nenhum", ou seja, está afogado e então perde o jogo.
Clicando em /Mapa/ e /Jogador/ é possivel obter mais informações relativamente a estas funções.

@
>>> jogoTerminou2 (Mapa 5 [(Rio (-2), [Nenhum, Tronco, Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (3,0))
True
@

@
>>> jogoTerminou2 (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Tronco, Tronco, Nenhum])])(Jogador (3,1))))
False
@
|-}

jogoTerminou2 :: Mapa -> Jogador-> Bool
jogoTerminou2 (Mapa l ((Rio v,o):t2)) (Jogador (x,y))
  |(!!) o x == Nenhum = True
  | otherwise = False

{- | A função "jogoTerminou3" testa se o jogador, quando está numa linha em que o terreno é Estrada, está numa coordenada "Carro", ou seja, está atropelado e então perde o jogo.
Clicando em /Mapa/ e /Jogador/ é possivel obter mais informações relativamente a estas funções.

@
>>> jogoTerminou3 (Mapa 5 [(Estrada 3, [Nenhum, Nenhum, Carro, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (3,0))
True
@

@
>>> jogoTerminou3 (Mapa 5 [(Estrada 3, [Nenhum, Nenhum, Carro, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (0,0))
False
@

|-}

jogoTerminou3 :: Mapa -> Jogador-> Bool
jogoTerminou3 (Mapa l (((Estrada v), (o)):t2)) (Jogador (x,y))
  |(!!) o x == Carro = True 
  | otherwise = False


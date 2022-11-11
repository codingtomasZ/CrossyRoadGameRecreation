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
1. Quando o mapa desliza e o jogador fica para tr ́as.
2. Quando o tronco em que o jogador se encontra eventualmente desapa-
rece do mapa.
Em ambas as situa ̧c ̃oes, o jogador perde e o jogo termina imediatemente.
Num mesmo rio ou estrada, troncos e carros deslocam-se numa direc ̧c ̃ao
comum e a uma velocidade constante. Eventualmente estes obst ́aculos sair ̃ao
do mapa por um dos lados, voltando a reaparecer no lado oposto, como se
os limites do mapa fossem wormholes.
-}



jogoTerminou :: Jogo -> Bool
jogoTerminou = if foradomapa == True && agua == True && undercarro == True 
               then True 
               else False 

foradoMapa :: Mapa -> Jogador -> Bool 
foradoMapa (Mapa n [(ter,l)]) (Jogador (a,b)) = if b < 0 || a < 0 || a>n 
                                                then False
                                                else True


agua :: 

undercarro :: 
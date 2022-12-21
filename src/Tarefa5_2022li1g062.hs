{- |
Module      : Tarefa5_2022li1g062
Description : Certificar a criação de uma nova linha de mapa e a eliminação de outra
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/2023.
-}

module Tarefa5_2022li1g062 where

import LI12223
import Tarefa2_2022li1g062
import Test.HUnit 

deslizaJogo :: Jogo -> Jogo
deslizaJogo (Jogo (Jogador (x,y)) (Mapa l (h:t))) = (Jogo (Jogador (x,y)) mapa_novo)
    where mapa_novo = estendeMapa (Mapa l (init (h:t))) (y+x)

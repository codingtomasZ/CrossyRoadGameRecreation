{- |
Module      : Tarefa2_2022li1g062
Description : Geração contínua de um mapa
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g062 where

import LI12223

{- estendeMapa :: Mapa -> Int -> Mapa
estendeMapa = undefined

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos = undefined

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos = undefined -}

{- Funcao proximosTerrenosValidos -}

proximosTerrenosValidos :: Mapa -> [Terreno] 
proximosTerrenosValidos (Mapa l []) = [Relva,Rio 0,Estrada 0]
proximosTerrenosValidos (Mapa l [(Rio v,a),(Rio)]) 

{- |
Module      : Tarefa2_2022li1g062
Description : Geração contínua de um mapa
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/2023.
-}

module Tarefa2_2022li1g062 where

import LI12223

import Test.HUnit 

{- |A função ’proximosTerrenosValidos’ calcula a lista de terrenos que poderao surgir na nova linha do mapa. Para esta funçao iremos ignorar os parametros relacionados com a velocidade do /terreno Estrada/ e /terreno Rio/.
Clicando em __Mapa__ e em __Terreno__ acima e possivel obter mais informaçoes relativamente a estas 2 funcoes.

== Exemplos de utilização:

@
>>> proximosTerrenosValidos (Mapa 3 [(Estrada 3,[])])
[Relva,Rio 0,Estrada 0]
@

@
>>>  proximosTerrenosValidos (Mapa 3 [(Estrada 3,[Carro,Carro,Nenhum]), (Relva , [Arvore,Arvore,Nenhum])])
[Relva,Rio 0,Estrada 0]
@

@
>>> proximosTerrenosValidos (Mapa 2 [(Rio 7, [Tronco,Nenhum]), (Rio 1, [Tronco,Nenhum]),(Rio 2, [Tronco,Nenhum]),(Rio 3, [Tronco,Nenhum])])
[Estrada 0,Relva]
@

-}


{- Funçao proximosTerrenosValidos -}

proximosTerrenosValidos :: Mapa -> [Terreno]  -- ^  
proximosTerrenosValidos (Mapa l []) = [Relva,Rio 0,Estrada 0] 
proximosTerrenosValidos (Mapa l [(Estrada v ,a),(Estrada vv ,aa),(Estrada vvv ,aaa),(Estrada vvvv ,aaaa),(Estrada vvvvv ,aaaaa)])=[Rio 0,Relva]
proximosTerrenosValidos (Mapa l (x:[(Estrada  v, a),(Estrada  vv, aa),(Estrada vvv, aaa),(Estrada vvvv, aaaa),(Estrada aaaaa, vvvvv)]))=[Rio 0, Relva]
proximosTerrenosValidos (Mapa l [(Rio v, a),(Rio vv, aa),(Rio vvv, aaa),(Rio vvvv, aaaa)]) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l (x:[(Rio v, a),(Rio vv, aa),(Rio vvv, aaa),(Rio vvvv, aaaa)])) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l [(Relva ,a),(Relva, aa),(Relva , aaa),(Relva , aaaa),(Relva , aaaaa)])=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa l (x:[(Relva ,a),(Relva, aa),(Relva , aaa),(Relva , aaaa),(Relva , aaaaa)]))=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa l t) = [Relva,Rio 0 ,Estrada 0]



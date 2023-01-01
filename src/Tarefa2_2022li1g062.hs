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
import System.Random
import Data.Numbers.Primes

{- | A funcao ’estendeMapa’ tem como finalidade gerar e adicionar uma nova linha valida ao topo de um dado mapa. O valor inteiro deve estar entre [0,100] usado para acrescentar alguma pseudo-aleatoriedade a geracao a proxima nova linha.
Acima encontram-se mais informaçoes sobre as funçoes /Mapa/. 

== Exemplos de utilização:

@
>>>  estendeMapa (Mapa 3 [(Relva , [])]) 3
Mapa 3 [(Relva,[]),(Relva,[Arvore,Arvore,Nenhum])]
@

== Sobre...

Na funçao 'estendeMapa', usamos random com recurso a funçao mod de modo a gerar novas linhas de terreno no mapa. 

-}

{- Funçao estendeMapa -}

estendeMapa :: Mapa -> Int -> Mapa 
estendeMapa (Mapa l m) n = ( Mapa l ((terreno_selecionado, (listaObstaculos l n (terreno_selecionado, []))):m ))
    where terreno_selecionado = terrenos_validos !! mod n (length terrenos_validos)
          terrenos_validos = if velocidade_positiva (head m) == True then (proximosTerrenosValidos (-d) (Mapa l m)) else (proximosTerrenosValidos d (Mapa l m))
          d = escolha_velocidade n


velocidade_positiva :: (Terreno, [Obstaculo]) -> Bool
velocidade_positiva (Relva, obs) = False
velocidade_positiva (Estrada v, obs) = if v > 0 then True else False
velocidade_positiva (Rio v, obs) = if v > 0 then True else False  


{-|


-}

escolha_velocidade :: Int -> Int
escolha_velocidade n
      | (even (head random_num) == True) = if mod ( head random_num) (4) == 0 then 1 else 3
      | otherwise =  if isPrime (head random_num) == True then 4 else 2
             where random_num = randomIntsL n 1 


{-|


-}


listaObstaculos :: Int -> Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
listaObstaculos l n (ter,o) 
      |length o == l = []
      |length o == (l-1) = [Nenhum]
      |odd (last (randomIntsL n l)) = (head obstaculos_validos):(listaObstaculos l (n+1) (ter,(head obstaculos_validos):o))
      |otherwise = (last obstaculos_validos):(listaObstaculos l (n+1) (ter,(last obstaculos_validos):o))
            where obstaculos_validos = proximosObstaculosValidos l (ter, o) 


{-|


-}


randomIntsL :: Int -> Int -> [Int]
randomIntsL seed l = take l $ randoms (mkStdGen seed)


{-
listaObstaculos :: Int -> Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
listaObstaculos l n (ter,o)
 |length o == l = o
 |length o ==(l-1) && (elem Nenhum o == False) = o ++ [Nenhum]
 |otherwise = listaObstaculos l (div ((n+1)^4) 3) (ter,[(obstaculos_validos !! mod n (length obstaculos_validos))] ++ o)
      where obstaculos_validos = proximosObstaculosValidos l (ter, o) 
-}

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


proximosTerrenosValidos :: Velocidade -> Mapa -> [Terreno]  
proximosTerrenosValidos v (Mapa l []) = [Relva, Rio v, Estrada v] 
proximosTerrenosValidos v (Mapa l [((Estrada v1) , a),(Estrada vv ,aa),(Estrada vvv ,aaa),(Estrada vvvv ,aaaa),(Estrada vvvvv ,aaaaa)])=[Rio v,Relva]
proximosTerrenosValidos v (Mapa l (x:[((Estrada v1 ), a),(Estrada  vv, aa),(Estrada vvv, aaa),(Estrada vvvv, aaaa),(Estrada aaaaa, vvvvv)]))=[Rio v, Relva]
proximosTerrenosValidos v (Mapa l [((Rio v1) , a),(Rio vv, aa),(Rio vvv, aaa),(Rio vvvv, aaaa)]) = [Estrada v, Relva]
proximosTerrenosValidos v (Mapa l (x:[((Rio v1) , a),(Rio vv, aa),(Rio vvv, aaa),(Rio vvvv, aaaa)])) = [Estrada v, Relva]
proximosTerrenosValidos v (Mapa l [(Relva ,a),(Relva, aa),(Relva , aaa),(Relva , aaaa),(Relva , aaaaa)])=[Estrada v, Rio v]
proximosTerrenosValidos v (Mapa l (x:[(Relva ,a),(Relva, aa),(Relva , aaa),(Relva , aaaa),(Relva , aaaaa)]))=[Estrada v, Rio v]
proximosTerrenosValidos v (Mapa l t) = [Relva, Rio v ,Estrada v]


{- | A função ’proximosObauxiliar’ calcula os obstaculos que podem ser gerados para continuar uma dada linha do mapa. O valor inteiro corresponde a largura do mapa. Se o comprimento da lista de obstaculos atinge a largura do mapa entao mais nenhum obstaculo e possivel adicionar. Os obstaculos escolhidos devem ainda estar de acordo com o seu respetivo terreno.
Clicando em /Terreno/ e /Obstaculo/ é possivel obter mais informaçoes relativamente a estas funcoes. 

== Exemplos de utilização:

@
>>> proximosObauxiliar 10 (Estrada 3 , [Carro,Carro,Carro,Carro])
[Nenhum]
@

@
>>>  proximosObauxiliar 3 (Estrada 3 , [Carro,Carro,Carro,Nenhum])
[Nenhum] 
@

@
>>>  proximosObauxiliar 6 (Estrada 3 , [Carro,Carro,Carro,Carro])
[Nenhum]
@

@
>>>  proximosObauxiliar 6 (Estrada 3 , [])
[Nenhum,Carro]
@

@
>>>  proximosObauxiliar 2 (Estrada 3 , [Carro,Carro,Carro,Carro])
[Nenhum]
@

-}

{- Funçao proximosObstaculosValidos -}


proximosObstaculosValidos  :: Int -> (Terreno,[Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos n (Estrada v, []) = [Nenhum,Carro]
proximosObstaculosValidos n (Rio v, []) = [Nenhum, Tronco]
proximosObstaculosValidos n (Rio v, [Tronco, Tronco, Tronco, Tronco, Tronco]) = [Nenhum]
proximosObstaculosValidos n (Rio v, lar) = [Nenhum,Tronco]
proximosObstaculosValidos n (Relva, lar) = [Nenhum,Arvore]
proximosObstaculosValidos n (Estrada v, [Carro, Carro, Carro]) = [Nenhum]
proximosObstaculosValidos n (Estrada v, lar) = [Nenhum,Carro]
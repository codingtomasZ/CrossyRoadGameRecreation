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

{- | A funcao ’estendeMapa’ tem como finalidade gerar e adicionar uma nova linha valida ao topo de um dado mapa. Esta função irá inicialmente escolher um terreno com uso da função "mod" entre o numero dado e o comprimento da lista de obstáculos. A partir do resultado dessa aplicação o terreno sera escolhido e de seguida, a partir desse terreno, será gerada a lista de obstáculos.
Acima encontram-se mais informaçoes sobre as funçoes /Mapa/. 

== Exemplos de utilização:

@
>>>  estendeMapa (Mapa 3 [(Relva , [])]) 3
Mapa 3 [(Relva,[]),(Relva,[Arvore,Arvore,Nenhum])]
@

== Sobre...

Na funçao 'estendeMapa', usamos random com recurso a funçao mod de modo a escolher o terreno da nova linha. 

-}

{- Funçao estendeMapa -}

estendeMapa :: Mapa -> Int -> Mapa 
estendeMapa (Mapa l m) n = ( Mapa l ((terreno_selecionado, (listaObstaculos l n (terreno_selecionado, []))):m ))
    where terreno_selecionado = terrenos_validos !! mod n (length terrenos_validos)
          terrenos_validos = if velocidade_positiva (head m) == True then (proximosTerrenosValidos (-d) (Mapa l m)) else (proximosTerrenosValidos d (Mapa l m))
          d = escolha_velocidade n


{-| A função velocidade positiva vai avaliar se a velocidade de uma dada linha é positiva. Esta função auxiliara a função "estendeMapa". Caso a velocidade for positiva, a função retornara o valor "True", caso não seja, o resutado sera "False". No caso do terreno ser Relva, ou seja, sem velocidade, o resultado será também "False".

== Exemplos de utilização:

@
>>>  velocidade_positiva (Estrada (-2), [Carro, Nenhum, Nenhum])
False 
@

@
>>>  velocidade_positiva (Rio (2), [Nenhum, Nenhum, Tronco, Tronco, Nenhum])
True 
@
-}

velocidade_positiva :: (Terreno, [Obstaculo]) -> Bool
velocidade_positiva (Relva, obs) = False
velocidade_positiva (Estrada v, obs) = if v > 0 then True else False
velocidade_positiva (Rio v, obs) = if v > 0 then True else False  


{-| A funcao escolha_velocidade vai escolher uma velocidade de forma pseudo-aleatoria. As velocidades que esta funcao pode possivelmente retornar vao de 1 a 4. Esta função vai receber um numero e apartir desse numero vai ser gerada uma lista de um número pseudo-aleatorio gerada a partir do numero inicial. Caso esse tal numero gerado na lista seja par e divisivel por 4, a velocidade escolhida será "1". Caso o numero gerado seja par mas não divisivel por 4, a velocidade escolhida será "3". Caso o número gerado seja impar, a velocidade escolhida será 4 caso esse numero seja primo, ou 2 caso não seja. 

== Exemplos de utilização:

@
>>> escolha_velocidade 24 
1
@

@
>>> escolha_velocidade 55
3
@

@
>>> escolha_velocidade 2
2
@

-}

escolha_velocidade :: Int -> Int
escolha_velocidade n
      | (even (head random_num) == True) = if mod ( head random_num) (4) == 0 then 1 else 3
      | otherwise =  if isPrime (head random_num) == True then 4 else 2
             where random_num = randomIntsL n 1 


{-| A funcao listaObstaculos vai gerar uma nova lista de obstáculos a partir da largura pretendida para a lista, de um inteiro dado e um terreno, associado a uma lista de obstaculos vazia ou não. O inteiro dado irá gerar uma lista de números pseudo-aleatórios. O ultimo elemento dessa lista é analisado em relacão à sua paridado visto que, caso seja impar, o obstaculo escolhido sera o primeiro de uma lista previamente fornecida dos obstaculos que são validos para o terreno escolhido. Caso seja par, sera escolhido o segundo/ultimo elemento dessa mesma lista. 

@
>>> listaObstaculos 5 34 (Estrada 2, [])
[Nenhum,Carro,Carro,Nenhum,Nenhum]
@

-}


listaObstaculos :: Int -> Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
listaObstaculos l n (ter,o) 
      |length o == l = []
      |length o == (l-1) = [Nenhum]
      |odd (last (randomIntsL n l)) = (head obstaculos_validos):(listaObstaculos l (n+1) (ter,(head obstaculos_validos):o))
      |otherwise = (last obstaculos_validos):(listaObstaculos l (n+1) (ter,(last obstaculos_validos):o))
            where obstaculos_validos = proximosObstaculosValidos l (ter, o) 


{-| A funcao randomIntsL vai receber dois inteiros de modo a criar um lista de numeros pseudo-aleatorios. Esta funcao vai receber um inteiro que dara origem a lista de numeros e um segundo inteiro que está associado a funcao "take", que determinara o comprimento da lista de numeros gerdos.

@
>>>  randomIntsL 344 1
[5105422989647308011]
@

-}


randomIntsL :: Int -> Int -> [Int]
randomIntsL seed l = take l $ randoms (mkStdGen seed)



{- |A função ’proximosTerrenosValidos’ calcula a lista de terrenos que poderao surgir na nova linha do mapa. Também será atribuida uma velocidade previamente escolhida a estes terrenos.
Clicando em __Mapa__ e em __Terreno__ acima e possivel obter mais informaçoes relativamente a estas 2 funcoes.

== Exemplos de utilização:

@
>>> proximosTerrenosValidos (-3) (Mapa 3 [(Estrada 3,[])])
[Relva,Rio (-3),Estrada (-3)]
@

@
>>>  proximosTerrenosValidos 2 (Mapa 3 [(Estrada 3,[Carro,Carro,Nenhum]), (Relva , [Arvore,Arvore,Nenhum])])
[Relva,Rio 2,Estrada 2]
@

@
>>> proximosTerrenosValidos 1 (Mapa 2 [(Rio 7, [Tronco,Nenhum]), (Rio 1, [Tronco,Nenhum]),(Rio 2, [Tronco,Nenhum]),(Rio 3, [Tronco,Nenhum])])
[Estrada 1,Relva]
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
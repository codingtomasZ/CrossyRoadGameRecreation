
{- |
Module      : Tarefa1_2022li1g062
Description : Validação de um mapa
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/2023.
-}
module Tarefa1_2022li1g062 where
import LI12223

-- Tarefa 1


{-|
Funçao 'mapaValido'. Valida que os obstaculos estejam de acordo com o terreno que recebem. Rios contiguos possuem direçoes opostas. Troncos tem no maximo 5 unidades de comprimento. Carros tem no maximo 3 unidades de comprimento. Nenhum pertence a lista de obstaculos para qualquer terreno em qualquer que seja a linha. Largura e igual ao comprimento da lista de obstaculos. Nao devem existir mais do que 4 Rio seguidos. Nao devem existir mais do que 5 Estrada seguidas. Nao devem existir mais de 5 Relva seguidas.

-}


mapaValido :: Mapa -> Bool 
mapaValido (Mapa l []) = True 
mapaValido m = if mapaValido1 m == True && mapaValido2 m == True && mapaValido3 m == True && mapaValido4 m== True && mapaValido5 m== True && mapaValido6 m== True && mapaValido7 m== True then True else False 

{- |
A função ’mapaValido1' tem como objetivo delimitar os obstaculos nos devidos terrenos.
Ou seja, caso Tronco ou Carro pertençam ao terreno Relva a funçao retornara Falso e caso contrario retornara True.
O mesmo acontece para Carro ou Arvore pertencer a Rio e ainda para Arvore ou Tronco a pertencer ao terreno Estrada. 
A funçao analisa sempre os obstaculos da primeira lista e depois parte para as restantes listas. Exemplo: caso a primeira lista apresente terreno Relva apenas com Arvore, esta entao analisa a segunda lista. Iremos supor que a segunda lista apresenta terreno Estrada v apenas com Carro, entao esta ira partir para a terceira lista. Caso uma lista apresente um obstaculo nao valido para o terreno apropriado entao a funçao retornara False. Ate la a funçao ira analisar todas as listas ate chegar a ultima lista (lista vazia) onde retornara True pois todas as listas anteriores foram aprovadas.

== Exemplos de utilizaçao:

@
>>>  mapaValido1 (Mapa 3 [(Estrada 2, [Arvore, Carro, Nenhum])])]
False
@

@
>>>  mapaValido1 (Mapa 3 [(Rio 2, [Arvore, Tronco, Nenhum])])]
False
@

-}


-- Exercicio 1 

mapaValido1 :: Mapa -> Bool 
mapaValido1 (Mapa l []) = True 
mapaValido1 (Mapa l ((Relva, o):t))
 | (elem Tronco o || elem Carro o) = False 
 | otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Rio v, o):t))
 | (elem Arvore o || elem Carro o) = False 
 | otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Estrada v, o):t))
 | (elem Tronco o || elem Arvore o) = False 
 | otherwise = mapaValido1 (Mapa l t)


{- | A funçao 'mapaValido2' analisa as direçoes opostas em rios contiguos.
Nesta funçao aplicamos a restriçao de que, para 2 rios contiguos, ( ((Rio v, o):(Rio v', o'):t) ) , para terem direçoes opostas, entao o sinal de v' tem de ser diferente do sinal de v', isto e, ou v e positivo e v negativo ou vice-versa, v negativo e v' positivo. Logo, quando o sinal de v' e igual ao sinal de v a funçao retorna False... isto e, a funçao retorna False caso v seja negativo e v' negativo ou quando v' positivo e v positivo.
Caso as restriçoes sejam aplicadas no Mapa dado entao a funçao retorna False, em caso contrario a funçao percorre todas as listas ate chegar a lista vazia retornando True. 
Caso a funçao inicie por Estrada ou Relva entao esta ira buscar os restantes terrenos, podendo encontrar o caso em que v e v' tem sinais iguais retornando False ou entao, nao encontrando este caso restriçao, a funçao acaba por encontrar a lista vazia dirigindo ao caso de paragem retornando True.
Caso haja Estrada pos Rio ou Relva pos Rio, entao a funçao ira buscar os restantes terrenos, podendo ou nao encontrar a restriçao. A restriçao nao e aplicada nos casos da Estrada pos Rio ou Relva pos Rio, pois nestes casos os rios nao sao contiguos.
O caso em que ( ((Rio v,o):[]) ) corresponde quando a funçao ja atravessou todas as listas e recebe um Rio e a lista final ( lista vazia ) e se a funçao atravessou todas as listas e nao "parou" nas restriçoes entao e porque a funçao foi valida e entao retorna True.

== Exemplos de utilizaçao: 

@
>>>  mapaValido2 (Mapa 2 [(Rio 2, [Tronco, Nenhum]), (Rio 2 , [Tronco, Nenhum])])]
False 
@

-}

-- Exercicio 2

mapaValido2 :: Mapa -> Bool 
mapaValido2 (Mapa l []) = True 
mapaValido2 (Mapa l ((Estrada v, o):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Relva, o):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v, o):(Relva, o'):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v,o):(Estrada v',o'):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v,o):[])) = True 
mapaValido2 (Mapa l ((Rio v, o):(Rio v', o'):t))
 | (v>0 && v'>0) || (v<0) && (v'<0) = False 
 | otherwise = mapaValido2 (Mapa l ((Rio v', o'):t))

{-| Para a funçao 'mapaValido3' foi utilizado um contador que começa pelo Int 0.
O contador ira aumentar +1 sempre que o proximo obstaculo e Tronco e caso este seja diferente de Tronco entao o contador reinicia (torna a 0) e devolve o resto da lista e analisa-a.
Isto ira detetar quando ha mais do que 5 Tronco, que retornara False. Caso contrario, devolve True.

== Exemplos de utilizaçao:

@
>>>  mapaValido3 0 [Tronco , Tronco , Tronco , Tronco , Nenhum , Tronco , Tronco , Tronco , Tronco , Tronco , Tronco]
False 
@

@
>>>  mapaValido3 0 [Tronco , Tronco , Tronco , Tronco , Tronco]
True 
@

-}


mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa l []) = True
mapaValido3 (Mapa l ((Rio v, o):t))
  | (mapaValido3_1 0 o) == True = (mapaValido3 (Mapa l t))
  | (mapaValido3_1 0 o) == False = False
  | otherwise = mapaValido3 (Mapa l t )


{-|

Funçao auxiliar para 'mapaValido3' 

-}


mapaValido3_1 :: Int -> [Obstaculo] -> Bool 
mapaValido3_1 6 _ = False
mapaValido3_1 n [] = True 
mapaValido3_1 n (x:t) 
  | x == Tronco = mapaValido3_1 (n+1) t 
  | not (x == Tronco) = mapaValido3_1 0 t


{-| Para a funçao 'mapaValido4' foi utilizado um contador que começa pelo Int 0.
O contador ira aumentar +1 sempre que o proximo obstaculo e Carro e caso este seja diferente de Carro entao o contador reinicia (torna a 0) e devolve o resto da lista e analisa-a.
Isto ira detetar quando ha mais do que 3 Carro, que retornara False. Caso contrario, devolve True.

== Exemplos de utilizaçao:

@
>>>  mapaValido4 0 [Carro , Carro , Carro, Nenhum, Carro , Carro , Carro]
True
@

@
>>> mapaValido4 0 [Carro, Carro, Nenhum , Carro , Nenhum , Carro , Carro , Carro , Carro]
False 
@

-}

mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa l []) = True
mapaValido4 (Mapa l ((Estrada v, o):t))
  | (mapaValido4_1 0 o) == True = (mapaValido4 (Mapa l t))
  | (mapaValido4_1 0 o) == False = False
  | otherwise = mapaValido4 (Mapa l t)

{-|

Funçao auxiliar para 'mapaValido4'

-}



mapaValido4_1 :: Int -> [Obstaculo] -> Bool 
mapaValido4_1 4 _ = False
mapaValido4_1 n [] = True 
mapaValido4_1 n (x:t) 
  | x == Carro = mapaValido4_1 (n+1) t 
  | not (x == Carro) = mapaValido4_1 0 t



{-| 
A funçao 'mapaValido5' indica quando ha pelo menos um Obstaculo Nenhum na lista de obstaculos.
A funçao começa por analisar os primeiros Obstaculos atribuidos na primeira lista constituida pelo Terreno e os devidos obstaculos.
Caso a primeira lista nao possua qualquer Nenhum ( elem Nenhum (x:y) == False = False ) a funçao retornara imediatamente False.
Caso na primeira lista, na parte dos obstaculos possua pelo menos um Nenhum (otherwise) a funçao analisara a tail da lista (os restantes terrenos e obstaculos do mapa).
A funçao fara entao o mesmo que ja foi referido. Analisa a lista e retorna imediatamente False ao detetar que nao ha qualquer Nenhum na lista de Obstaculo.
Caso True, a funçao percorre todas as listas e quando chega a ultima lista (lista vazia) passando ja por todas as outras retorna True, pois o mapa sera valido nestas condiçoes ( mapaValido5 (Mapa l []) = True )

-}
-- Exercicio 5

mapaValido5 :: Mapa -> Bool 
mapaValido5 (Mapa l []) = True 
mapaValido5 (Mapa l ((p , (x:y)):t))
 | elem Nenhum (x:y) == False = False 
 | otherwise = mapaValido5 (Mapa l t)

{-| A funçao 'mapaValido6' analisa se a largura do mapa e igual ao comprimento da lista de obstaculos.
Ou seja, se o comprimento (length) da lista de obstaculos (h:t) for diferente de lar (largura do mapa) a funçao retornara False.
Caso contrario, isto e, se na primeira lista o comprimento da lista de obstaculos for igual a largura do mapa, entao a funçao ira analisar as restantes listas de Terreno e Obstaculo e fazer o mesmo processo ate encontrar uma lista cujo comprimento da lista de obstaculos seja diferente da largura do mapa retornando False.
Caso todas as funçoes respeitem a regra do comprimento da lista de obstacaulos ser igual a largura do mapa, a funçao analisara a ultima lista (lista vazia) e, por definiçao da funcao definida, a analise da lista vazia corresponde a True.

== Exemplos de utilizaçao: 

@
>>>  mapaValido6 (Mapa 3 [(Relva , [Arvore, Nenhum])])]
False 
@

-}


-- Exercicio 6 

mapaValido6 :: Mapa -> Bool 
mapaValido6 (Mapa l []) = True 
mapaValido6 (Mapa l ((p,(h:t)):t2))
 | not (length (h:t) == l) = False 
 | otherwise = mapaValido6 (Mapa l t2)


{-| A funçao 'mapaValido7' analisa que nao devem existir mais do que 4 rios contiguos nem mais do que 5 estradas ou relvas contiguas.
Nesta funçao aplicamos as seguintes restriçoes => Terreno Rio, 5 vezes seguidas seguido de qualquer Terreno => False (pois podem existir apenas 4 rios contiguos no maximo); => Terreno Estrada, 6 vezes seguidas seguido de qualquer Terreno => False (pois podem existir apenas 5 estradas contiguas no maximo); => Terreno Relva, 6 vezes seguidas seguido de qualquer Terreno => False (pois podem existir apenas 5 relvas contiguas no maximo.
Caso o Mapa atribuido assuma as restriçoes entao a funçao retornara False.
Caso contrario a funçao analisa o Terreno de cada lista e caso as restriçoes nao estejam presentes a funçao acabara por analisar a lista vazia que corresponde a ultima lista da funçao Mapa e retornara True, pois ja analisou todas as outras listas anteriores que nao continham restriçoes.

== Exemplos de utilizaçao: 

@
>>> mapaValido7 (Mapa 2 [(Relva , [Arvore, Nenhum]) , (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]) ])
False 
@

-}

-- Contiguamente, n ̃ao devem existir mais do que 4 rios, nem 5 estradas ou relvas.

-- Exercicio 7

mapaValido7 :: Mapa -> Bool 
mapaValido7 (Mapa l []) = True 
mapaValido7 (Mapa l ((Rio v,(h1:t1)):(Rio v1,(h2:t2)):(Rio v2,(h3:t3)):(Rio v3,(h4:t4)):(Rio v4,(h5:t5)):t6)) = False
mapaValido7 (Mapa l ((Estrada v,(h1:t1)):(Estrada v1,(h2:t2)):(Estrada v2,(h3:t3)):(Estrada v3,(h4:t4)):(Estrada v4,(h5:t5)):(Estrada v5,(h6:t6)):t7)) = False
mapaValido7 (Mapa l ((Relva,(h1:t1)):(Relva,(h2:t2)):(Relva,(h3:t3)):(Relva,(h4:t4)):(Relva,(h5:t5)):(Relva,(h6:t6)):t7)) = False
mapaValido7 (Mapa l ((t,(h1:t1)):t2)) = mapaValido7 (Mapa l t2)



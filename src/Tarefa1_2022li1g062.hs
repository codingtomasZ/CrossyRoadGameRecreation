
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

mapaValido :: Mapa -> Bool 
mapaValido (Mapa l []) = True 
mapaValido m = if mapaValido1 m == True && mapaValido2 m == True && mapaValido34 m == True && mapaValido5 m == True && mapaValido6 m == True && mapaValido7 == True 
               then True 
               else False

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

{-

--Exercício 3 (Imcompleto) e 4

mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa l []) = True 
mapaValido3 m = aux m 0

aux :: Mapa -> Int -> Bool
aux (Mapa l (((Rio v),[]):t2)) x = True 
aux (Mapa l (((Rio v),(h1:t1)):t2)) x 
  | h1 == Tronco = aux (Mapa l (((Rio v),t1):t2)) (x+1)
  | x > 5 = False
  | otherwise = aux (Mapa l (((Rio v),t1):t2)) x

-}

-- Exercicio 5

mapaValido5 :: Mapa -> Bool 
mapaValido5 (Mapa l []) = True 
mapaValido5 (Mapa l ((t,o):t2))
 | (elem (Nenhum o) == False) = False 
 | otherwise = mapaValido5 (Mapa l t2)

-- Exercicio 6 

mapaValido6 :: Mapa -> Bool 
mapaValido6 (Mapa l []) = True 
mapaValido6 (Mapa l ((p,(h1:t1)):t2))
 | length (h1:t1) /= l = False 
 | otherwise = mapaValido6 (Mapa l t2)

-- Exercicio 7

mapaValido7 :: Mapa -> Bool 
mapaValido7 (Mapa l []) = True 
mapaValido7 (Mapa l ((Rio v,(h1:t1)):(Rio v1,(h2:t2)):(Rio v2,(h3:t3)):(Rio v3,(h4:t4)):(Rio v4,(h5:t5)):t6)) = False
mapaValido7 (Mapa l ((Estrada v,(h1:t1)):(Estrada v1,(h2:t2)):(Estrada v2,(h3:t3)):(Estrada v3,(h4:t4)):(Estrada v4,(h5:t5)):(Estrada v5,(h6:t6)):t7)) = False
mapaValido7 (Mapa l ((Relva,(h1:t1)):(Relva,(h2:t2)):(Relva,(h3:t3)):(Relva,(h4:t4)):(Relva,(h5:t5)):(Relva,(h6:t6)):t7)) = False
mapaValido7 (Mapa l ((t,(h1:t1)):t2)) = mapaValido7 (Mapa l t2)



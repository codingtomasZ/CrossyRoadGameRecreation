{- |
Module      : Tarefa1_2022li1g062
Description : Validação de um mapa
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g062 where

import LI12223
 

mapaValido :: Mapa -> Bool
mapaValido (Mapa l []) = True

mapaValido (Mapa l ((Relva,(h1:t1)):t2)) 
  |(elem Tronco (h1:t1) || elem Carro (h1:t1)) = False
  |otherwise = mapaValido (Mapa l t2)

mapaValido (Mapa l (((Rio v),(h1:t1)):t2))
  |(elem Carro (h1:t1) || elem Arvore (h1:t1)) = False
  |otherwise = mapaValido (Mapa l t2)
mapaValido (Mapa l (((Rio v),o):[])) = True
mapaValido (Mapa l (((Rio v),o):((Rio v'),o'):t2))
   | v > 0 && v' > 0 = False
   | v < 0 && v' < 0 = False
   |otherwise = mapaValido (Mapa l (((Rio v'),o'):t2))
mapaValido (Mapa l (((Estrada v),o):((Rio v'),o'):t2)) = True
mapaValido (Mapa l (((Relva),o):((Rio v'),o'):t2)) = True
mapaValido (Mapa l (((Rio v),o):((Estrada v'),o'):t2)) = True
mapaValido (Mapa l (((Rio v),o):((Relva),o'):t2)) = True

mapaValido (Mapa l (((Estrada v),(h1:t1)):t2))
  |(elem Arvore (h1:t1) || elem Tronco (h1:t1)) = False
  |otherwise = mapaValido (Mapa l t2)



{-mapaValido3 :: Mapa -> Bool
mapaValido3 m = aux m 0

aux :: Eq a => Mapa -> x -> Bool
aux (Mapa l (((Rio v),[]):t2)) x = True 
aux (Mapa l (((Rio v),(h1:t1)):t2)) x
   | x > 5 = False
   | h1 == Tronco = aux (Mapa l (((Rio v),(t1)):t2)) (x+1)-}
mapaValido5 (Mapa l []) = True
mapaValido5 (Mapa l (((Rio v),(h1:t1)):t2))
         | (elem Carro (h1:t1) || elem Tronco (h1:t1) || elem Arvore (h1:t1)) = False 
         | otherwise = mapaValido (Mapa l t2) 
mapaValido5' (Mapa l (((Relva),(h1:t1)):t2))
         | (elem Carro (h1:t1) || elem Tronco (h1:t1) || elem Arvore (h1:t1)) = False 
         | otherwise = mapaValido5' (Mapa l t2) 
mapaValido5'' (Mapa l (((Estrada v),(h1:t1)):t2))
         | (elem Carro (h1:t1) || elem Tronco (h1:t1) || elem Arvore (h1:t1)) = False 
         | otherwise = mapaValido5'' (Mapa l t2)  

mapaValido6 (Mapa l []) = True
mapaValido6 (Mapa l (((Estrada v),(h1:t1)):t2))
          | l == length (h1:t1) = True 
          | otherwise = False 
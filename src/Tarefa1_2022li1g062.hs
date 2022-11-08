
{- |
Module      : Tarefa1_2022li1g062
Description : Validação de um mapa
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/2023.
-}
module Tarefa1_2022li1g062 where

import LI12223

{- Tarefa 1 -}

mapaValido :: Mapa -> Bool 
mapaValido (Mapa l []) = True 
mapaValido (Mapa l ((p,(x:xs)):t)) = mapaValido1 (Mapa l ((p,(x:xs)):t)) && mapaValido2 (Mapa l ((p,(x:xs)):t)) && mapaValido34 (Mapa l ((p,(x:xs)):t)) && mapaValido5 (Mapa l ((p,(x:xs)):t)) && mapaValido6 (Mapa l ((p,(x:xs)):t)) && mapaValido7 (Mapa l ((p,(x:xs)):t))

{- Exercicio 1 -}

mapaValido1 :: Mapa -> Bool 
mapaValido1 (Mapa l []) = True 
mapaValido1 (Mapa l ((Relva,(x:xs)):t))
 | (elem Tronco (x:xs) || elem Carro (x:xs)) = False 
 | otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Rio v,(x:xs)):t))
 | (elem Arvore (x:xs) || elem Carro (x:xs)) = False 
 | otherwise = mapaValido1 (Mapa l t)
mapaValido1 (Mapa l ((Estrada v,(x:xs)):t))
 | (elem Tronco (x:xs) || elem Arvore (x:xs)) = False 
 | otherwise = mapaValido1 (Mapa l t)

{- Exercicio 2 -}

mapaValido2 :: Mapa -> Bool 
mapaValido2 (Mapa l []) = True 
mapaValido2 (Mapa l ((Estrada v,(x:xs)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Relva,(x:xs)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v,(x:xs)):(Relva,(x1:xs1)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v,(x:xs)):(Estrada v1,(x1:xs1)):t)) = mapaValido2 (Mapa l t)
mapaValido2 (Mapa l ((Rio v,(x:xs)):[])) = True 
mapaValido2 (Mapa l ((Rio v,(x:xs)):(Rio v1,(x1:xs1)):t))
 | (v>0 && v1>0) || (v<0) && (v1<0) = False 
 | otherwise = mapaValido2 (Mapa l ((Rio v1,(x1:xs1)):t))

{- Auxiliares -}

mapaValido34 :: Mapa -> Bool 
mapaValido34 (Mapa l []) = True 
mapaValido34 (Mapa l ((Relva,(x:xs)):t)) = mapaValido34 (Mapa l t)
mapaValido34 (Mapa l ((Rio v,(x:xs)):t))
 | mapaValido3 (Mapa l ((Rio v,(x:xs)):t)) == True = mapaValido (Mapa l t)
 | otherwise = False 
mapaValido34 (Mapa l ((Estrada v,(x:xs)):t))
 | mapaValido4 (Mapa l ((Estrada v,(x:xs)):t)) == True = mapaValido (Mapa l t)
 | otherwise = False 

{- Exercicio 3 -}

mapaValido3 :: Mapa -> Bool 
mapaValido3 (Mapa l ((Rio v,[]):t)) = True 
mapaValido3 (Mapa l ((Rio v,(x:xs)):t))
 | length (x:xs) <= 5 = True 
 | x == Tronco && head xs == Tronco && head (tail xs) == Tronco && head (tail(tail xs)) == Tronco && head (tail(tail(tail xs))) == Tronco && head (tail(tail(tail(tail xs)))) ==Tronco = False
 | otherwise = mapaValido3 (Mapa l ((Rio v,xs):t))

{- Exercicio 4 -}

mapaValido4 :: Mapa -> Bool 
mapaValido4 (Mapa l ((Estrada v,[]):t)) = True 
mapaValido4 (Mapa l ((Estrada v,(x:xs)):t))
 | length (x:xs) <= 3 = True 
 | x == Carro && head xs == Carro && head(tail xs) == Carro && head(tail(tail xs)) == Carro = False 
 | otherwise = mapaValido4 (Mapa l ((Estrada v,xs):t))

{- Exercicio 5 -}

mapaValido5 :: Mapa -> Bool 
mapaValido5 (Mapa l []) = True 
mapaValido5 (Mapa l ((p,(x:xs)):t))
 | elem Nenhum (x:xs) == False = False 
 | otherwise = mapaValido5 (Mapa l t)

{- Exercicio 6 -}

mapaValido6 :: Mapa -> Bool 
mapaValido6 (Mapa l []) = True 
mapaValido6 (Mapa l ((p,(x:xs)):t))
 | length (x:xs) /= l = False 
 | otherwise = mapaValido6 (Mapa l t)

{- Exercicio 7 -}

mapaValido7 :: Mapa -> Bool 
mapaValido7 (Mapa l []) = True 
mapaValido7 (Mapa l ((Rio v,(x:xs)):(Rio v1,(x1:xs1)):(Rio v2,(x2:xs2)):(Rio v3,(x3:xs3)):(Rio v4,(x4:xs4)):t)) = False
mapaValido7 (Mapa l ((Estrada v,(x:y)):(Estrada v1,(x1:xs1)):(Estrada v2,(x2:xs2)):(Estrada v3,(x3:xs3)):(Estrada v4,(x4:xs4)):(Estrada v5,(x5:xs5)):t)) = False
mapaValido7 (Mapa l ((Relva,(x:xs)):(Relva,(x1:xs1)):(Relva,(x2:xs2)):(Relva,(x3:xs3)):(Relva,(x4:xs4)):(Relva,(x5:xs5)):t)) = False
mapaValido7 (Mapa l ((p,(x:xs)):t)) = mapaValido7 (Mapa l t)


